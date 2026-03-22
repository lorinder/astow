{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict (runWriterT)
import Data.Text.IO (hPutStrLn)
import Options.Applicative
import System.Directory.OsPath (makeAbsolute)
import System.Exit
import System.IO (stderr)
import System.OsPath (osp, OsPath, encodeUtf, splitDirectories, takeDirectory, (</>))
import System.OsString (isPrefixOf)

import AstowMonadT
import Actions
import Diagnostic
import DirTree
import Fallible
import FileUtils (osPathToText)
import FsOps

import qualified KissDList as D

data Command =
      CmdStatus     [OsPath]
    | CmdPush       Bool [OsPath]
    | CmdPull       [OsPath]
    | CmdDelete     [OsPath]
    | CmdManifest   [OsPath]
    | CmdDiff       [OsPath]
    deriving (Show)

data CmdLine = CmdLine {
        clDebugLogFsOps     :: Bool
      , clDryRun            :: Bool
      , clStowDir           :: Maybe OsPath
      , clTargetDir         :: Maybe OsPath
      , clCmd               :: Command
    } deriving (Show)

statusParser :: Parser Command
statusParser = CmdStatus <$> many (argument osPathReader (metavar "DIRS..."))

pushParser :: Parser Command
pushParser = CmdPush
    <$> switch ( long "symlink"
        <> help "Symlink files into target instead of copying")
    <*> many (argument osPathReader (metavar "DIRS..."))

pullParser :: Parser Command
pullParser = CmdPull <$> many (argument osPathReader (metavar "DIRS..."))

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
    <*> optional (option osPathReader
        ( long "dir" <> short 'd'
        <> metavar "DIR"
        <> help "Stow directory (default: current directory)"))
    <*> optional (option osPathReader
        ( long "target" <> short 't'
        <> metavar "DIR"
        <> help "Target directory (default: parent of stow directory)"))
    <*> subparser
        ( command "status"
            (info (helper <*> statusParser) (progDesc "Sync status display"))
        <> command "push"
            (info (helper <*> pushParser) (progDesc "Copy packages to target"))
        <> command "pull"
            (info (helper <*> pullParser) (progDesc "Sync packages from target"))
        <> command "delete"
            (info (helper <*> deleteParser) (progDesc "Remove packages from target"))
        <> command "manifest"
            (info (helper <*> manifestParser) (progDesc "Show files manifest"))
        <> command "diff"
            (info (helper <*> diffParser) (progDesc "Diff packages against target"))
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
    stowDir <- makeAbsolute $ maybe [osp|.|] id (clStowDir cl)
    targetDir <- makeAbsolute $  maybe (takeDirectory stowDir) id (clTargetDir cl)
    let ac = ActionContext { acStowDir = stowDir, acTargetDir = targetDir }

    -- process
    let cmd :: (Monad m, MonadIO m, FsOps m) => AstowMonadT m ()
        cmd = case clCmd cl of
                CmdStatus files     -> runCmd ac statusIO files
                CmdPush sym files   -> runCmd ac (pushIO sym) files
                CmdPull files       -> runCmd ac pullIO files
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

runCmd :: (Monad m, FsOps m, MonadIO m)
    => ActionContext                                             -- ^ context
    -> (ActionContext -> [RootedDirTree ()] -> AstowMonadT m ())  -- ^ action
    -> [OsPath]                                                 -- ^ file args
    -> AstowMonadT m ()
runCmd ac actionFunc files = do
    -- Validate: reject user-supplied paths that contain ".." components.
    forM_ files $ \p ->
        when (any (== [osp|..|]) (splitDirectories p)) $ do
            tell1 $ Diagnostic
                ("Invalid argument: " <> osPathToText p)
                (TextPayload "path must not contain '..'")
                Error
            abort

    -- Create list of OsPaths.
    --
    -- If provided list is non-empty, use as is.
    -- Otherwise generate the list from the directory entries.
    let sd = acStowDir ac
    files' <- if not $ null files then
            return files
        else do
            d <- foListDirectory sd
                >>= filterM (\x -> foDoesDirectoryExist (sd </> x))
            return $ filter (not . (isPrefixOf [osp|.|])) d

    -- Scan RootedDirTree
    --
    -- For a simple name "p", root="p" and the whole directory is scanned.
    -- For a sub-path "p/a/b", root="p" and only the subtree at "p/a/b" is
    -- scanned, but it is wrapped back in the "a/b" skeleton so that paths
    -- within the tree remain relative to the root.
    --
    -- If the path does not exist in the stow directory but a sub-path was
    -- given, the corresponding path in the target directory is tried instead.
    -- This allows e.g. "astow pull bash/.bashrc" to capture a file that is
    -- not yet tracked in the stow directory.
    trees <- forM files' $ \fn ->
        case splitDirectories fn of
            [] -> abort  -- impossible: fn is a non-empty argument
            (root:sub) -> do
                let targetPath = foldl (</>) (acTargetDir ac) sub
                stowIsFile <- foDoesFileExist (sd </> fn)
                stowIsDir  <- foDoesDirectoryExist (sd </> fn)
                tr <- if stowIsFile || stowIsDir
                    then readFromFs (sd </> fn)
                    else case sub of
                        -- Top-level package name not in stow: hard error.
                        [] -> do
                            tell1 $ Diagnostic
                                ("Not found: " <> osPathToText fn)
                                (TextPayload "package not found in stow directory")
                                Error
                            abort
                        -- Sub-path: fall back to the target directory.
                        _ -> do
                            tgtIsFile <- foDoesFileExist targetPath
                            tgtIsDir  <- foDoesDirectoryExist targetPath
                            if tgtIsFile || tgtIsDir
                                then readFromFs targetPath
                                else do
                                    tell1 $ Diagnostic
                                        ("Not found: " <> osPathToText fn)
                                        (TextPayload "path not found in stow directory or target")
                                        Error
                                    abort
                return $ RootedDirTree root (mkLine sub tr)

    -- Execute action
    actionFunc ac trees
