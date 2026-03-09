{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict (runWriterT)
import Data.Text.IO (hPutStrLn)
import Options.Applicative
import System.Directory.OsPath (getCurrentDirectory)
import System.Exit
import System.IO (stderr)
import System.OsPath (osp, OsPath, encodeUtf, takeDirectory)
import System.OsString (isPrefixOf)

import AstowMonadT
import Actions
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
    | CmdDiff       [OsPath]
    deriving (Show)

data CmdLine = CmdLine {
        clDebugLogFsOps     :: Bool
      , clDryRun            :: Bool
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

diffParser :: Parser Command
diffParser = CmdDiff <$> many (argument osPathReader (metavar "DIRS..."))

cmdLineParser :: Parser CmdLine
cmdLineParser = CmdLine
    <$> switch ( long "debug-log-fsops"
        <> help "Log filesystem operations on stderr")
    <*> switch ( long "dry-run"
        <> help "Simulate operations without writing to disk")
    <*> subparser
        ( command "status"
            (info statusParser (progDesc "Sync status display"))
        <> command "push"
            (info pushParser (progDesc "Copy packages to target"))
        <> command "pull"
            (info pullParser (progDesc "Sync packages from target"))
        <> command "symlink"
            (info symlinkParser (progDesc "Symlink packages to target"))
        <> command "delete"
            (info deleteParser (progDesc "Remove packages from target"))
        <> command "manifest"
            (info manifestParser (progDesc "Show files manifest"))
        <> command "diff"
            (info diffParser (progDesc "Diff packages against target"))
        )

osPathReader :: ReadM OsPath
osPathReader = eitherReader $ \s ->
    case encodeUtf s of
        Left err -> Left (show err)
        Right p -> Right p

main :: IO ()
main = do
    -- Scan the command line
    cl <- execParser $ info (helper <*> cmdLineParser)
            ( fullDesc
                <> header "astow - copy based alternative to stow"
                <> progDesc ("Tool to manage a union of file system trees. "
                    ++ "Unlike stow, files are copied by default for"
                    ++ "reliability"))

    -- create action context
    curdir <- getCurrentDirectory
    let ac = ActionContext { acStowDir = curdir, acTargetDir = takeDirectory curdir }

    -- process
    let cmd :: (Monad m, MonadIO m, FsOps m) => AstowMonadT m ()
        cmd = case clCmd cl of
                CmdStatus files     -> runCmd ac statusIO files
                CmdPush files       -> runCmd ac pushIO files
                CmdPull files       -> runCmd ac pullIO files
                CmdSymlink files    -> runCmd ac symlinkIO files
                CmdDelete files     -> runCmd ac deleteIO files
                CmdManifest files   -> runCmd ac manifestIO files
                CmdDiff files       -> runCmd ac diffIO files
    (r, l) <- case (clDebugLogFsOps cl, clDryRun cl) of
                    (True,  True)  -> runLoggedFsOpsT $ runDryRunFsOpsT
                                        $ runWriterT $ runFallibleT
                                        $ runAstowMonadT cmd
                    (True,  False) -> runLoggedFsOpsT $ runWriterT $ runFallibleT
                                        $ runAstowMonadT cmd
                    (False, True)  -> runDryRunFsOpsT $ runWriterT $ runFallibleT
                                        $ runAstowMonadT cmd
                    (False, False) -> runWriterT $ runFallibleT $ runAstowMonadT cmd

    -- Print log
    forM_ (D.toList l) (\e ->
        hPutStrLn stderr $ diagnosticMessage e)

    -- Return
    exitWith (fallible (ExitFailure 1) (const (ExitFailure 2)) (const ExitSuccess) r)

    where
        runCmd :: (Monad m, FsOps m, MonadIO m)
            => ActionContext                                    -- ^ context
            -> (ActionContext -> [RootedDirTree ()] -> AstowMonadT m ())  -- ^ action
            -> [OsPath]                                         -- ^ file args
            -> AstowMonadT m ()
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
                tr <- readFromFs fn
                return $ RootedDirTree fn tr
                )

            -- Execute action
            actionFunc ac trees
