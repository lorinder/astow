module Main where

import System.Exit
import System.OsPath (encodeUtf)

import Control.Monad
import Options.Applicative

import Actions (RootedDirTree(..), status, push, pull, delete, symlink, manifest)
import DirTree

data Command =
      CmdStatus     [String]
    | CmdPush       [String]
    | CmdPull       [String]
    | CmdSymlink    [String]
    | CmdDelete     [String]
    | CmdManifest   [String]
    deriving (Show)

data CmdLine = CmdLine {
        clCmd               :: Command
    } deriving (Show)

statusParser :: Parser Command
statusParser = CmdStatus <$> many (argument str (metavar "DIRS..."))

pushParser :: Parser Command
pushParser = CmdPush <$> many (argument str (metavar "DIRS..."))

pullParser :: Parser Command
pullParser = CmdPull <$> many (argument str (metavar "DIRS..."))

symlinkParser :: Parser Command
symlinkParser = CmdSymlink <$> many (argument str (metavar "DIRS..."))

deleteParser :: Parser Command
deleteParser = CmdDelete <$> many (argument str (metavar "DIRS..."))

manifestParser :: Parser Command
manifestParser = CmdManifest <$> many (argument str (metavar "DIRS..."))

cmdLineParser :: Parser CmdLine
cmdLineParser = CmdLine <$> subparser
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

main :: IO ()
main = do
    -- Scan the command line
    cl <- execParser opts
    
    -- process
    r <- case clCmd cl of
        CmdStatus files -> runCmd status files
        CmdPush files -> runCmd push files
        CmdPull files -> runCmd pull files
        CmdSymlink files -> runCmd symlink files
        CmdDelete files -> runCmd delete files
        CmdManifest files -> runCmd manifest files

    -- Return
    exitWith (if r then ExitSuccess else ExitFailure 1)

    where
        opts = info (helper <*> cmdLineParser)
            ( fullDesc
                <> header "astow - minimal alternative to stow"
                <> progDesc ("Minimal alternative to the \"stow\" utility.  "
                    ++ "Manages a union of file system trees.  Unlike stow, "
                    ++ "files are copied by default, instead of a symlinked."))
        runCmd 
            :: ([RootedDirTree ()] -> IO Bool)  -- ^ action
            -> [String]                     -- ^ file args
            -> IO Bool
        runCmd actionFunc files = do
            trees <- forM files (\fn -> do
                d <- encodeUtf fn
                tr <- getDirTree d ()
                return $ RootedDirTree d tr
                )
            actionFunc trees
