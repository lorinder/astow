{-# LANGUAGE QuasiQuotes #-}

module Main where

import System.Exit
import System.OsPath (osp, OsPath, encodeUtf, takeDirectory)
import System.OsString (isPrefixOf)
import System.Directory.OsPath (getCurrentDirectory)
import System.IO

import Control.Monad
import Control.Monad.IO.Class
import Options.Applicative

import Actions (RootedDirTree(..),
        ActionContext(..),
        status, push, pull, delete, symlink, manifest)
import Diagnostic
import DirTree
import Fallible
import FsOps

import qualified KissDList as D

data Command =
      CmdStatus     [OsPath]
    | CmdPush       [OsPath]
    | CmdPull       [OsPath]
    | CmdSymlink    [OsPath]
    | CmdDelete     [OsPath]
    | CmdManifest   [OsPath]
    deriving (Show)

data CmdLine = CmdLine {
        clDebugLogFsOps     :: Bool
      , clCmd               :: Command
    } deriving (Show)

statusParser :: Parser Command
statusParser = CmdStatus <$> many (argument osPathReader (metavar "DIRS..."))

pushParser :: Parser Command
pushParser = CmdPush <$> many (argument osPathReader (metavar "DIRS..."))

pullParser :: Parser Command
pullParser = CmdPull <$> many (argument osPathReader (metavar "DIRS..."))

symlinkParser :: Parser Command
symlinkParser = CmdSymlink <$> many (argument osPathReader (metavar "DIRS..."))

deleteParser :: Parser Command
deleteParser = CmdDelete <$> many (argument osPathReader (metavar "DIRS..."))

manifestParser :: Parser Command
manifestParser = CmdManifest <$> many (argument osPathReader (metavar "DIRS..."))

cmdLineParser :: Parser CmdLine
cmdLineParser = CmdLine
    <$> switch ( long "debug-log-fsops"
        <> help "Log filesystem operations on stderr")
    <*> subparser
        ( command "status"
            (info statusParser (progDesc "Sync status display"))
        <> command "push"
            (info pushParser (progDesc "Push (copy) staging files to live"))
        <> command "pull"
            (info pullParser (progDesc "Pull (copy) staging files from live"))
        <> command "symlink"
            (info symlinkParser (progDesc "Symlink staging files to live"))
        <> command "delete"
            (info deleteParser (progDesc "Remove files from live"))
        <> command "manifest"
            (info manifestParser (progDesc "Show files manifest"))
        )

osPathReader :: ReadM OsPath
osPathReader = eitherReader $ \s ->
    case encodeUtf s of
        Left err -> Left (show err)
        Right p -> Right p

main :: IO ()
main = do
    -- Scan the command line
    cl <- execParser opts

    -- create action context
    curdir <- getCurrentDirectory
    let ac = ActionContext curdir (takeDirectory curdir)

    -- process
    let cmd :: (Monad m, MonadIO m, FsOps m) => FsOpsMonadT m Bool
        cmd = case clCmd cl of
                CmdStatus files     -> runCmd ac status files
                CmdPush files       -> runCmd ac push files
                CmdPull files       -> runCmd ac pull files
                CmdSymlink files    -> runCmd ac symlink files
                CmdDelete files     -> runCmd ac delete files
                CmdManifest files   -> runCmd ac manifest files
    (r, l) <- case clDebugLogFsOps cl of
                    True ->
                        (runLoggedFsOpsT . runFsOpsMonadT) (cmd :: FsOpsMonadT (LoggedFsOpsT IO) Bool)
                    False ->
                        runFsOpsMonadT (cmd :: FsOpsMonadT IO Bool)

    -- Print log
    forM_ (D.toList l) (\e ->
        hPutStrLn stderr $ diagnosticMessage e)

    -- Return
    exitWith (fallible (ExitFailure 1) (const (ExitFailure 2)) (const ExitSuccess) r)

    where
        opts = info (helper <*> cmdLineParser)
            ( fullDesc
                <> header "astow - minimal alternative to stow"
                <> progDesc ("Minimal alternative to the \"stow\" utility.  "
                    ++ "Manages a union of file system trees.  Unlike stow, "
                    ++ "files are copied by default, instead of a symlinked."))
        runCmd :: (Monad m, FsOps m, MonadIO m)
            => ActionContext                                    -- ^ context
            -> (ActionContext -> [RootedDirTree ()] -> FsOpsMonadT m Bool)  -- ^ action
            -> [OsPath]                                         -- ^ file args
            -> FsOpsMonadT m Bool
        runCmd ac actionFunc files = do
            -- Create list of OsPaths.
            --
            -- If provided list is non-empty, use as is.
            -- Otherwise generate the list from the directory entries.
            files' <- if not $ null files then
                    return files
                else do
                    d <- foListDirectory (acStowDir ac)
                        >>= filterM foDoesDirectoryExist
                    return $ filter (not . (isPrefixOf [osp|.|])) d

            -- Scan RootedDirTree
            trees <- forM files' (\fn -> do
                tr <- getDirTree fn ()
                return $ RootedDirTree fn tr
                )

            -- Execute action
            actionFunc ac trees
