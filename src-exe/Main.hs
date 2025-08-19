module Main where

import System.Exit
import System.IO
import System.OsPath (encodeUtf)

import Control.Monad
import Options.Applicative

import Actions (status, push, pull, delete, symlink)
import DirTree

data CmdLine = CmdLine {
        -- Actionflags
        clStatus            :: Bool
      , clPush              :: Bool
      , clPull              :: Bool
      , clDelete            :: Bool
      , clSymlink           :: Bool
    
        -- Directories to operate on
      , clDirs              :: [String]
    } deriving (Show)

cmdLineParser :: Parser CmdLine
cmdLineParser = CmdLine
    <$> flag False True (
            long "status"
            <> help "Compare staging to production tree" )
    <*> flag False True (
            long "push"
            <> help "Push (copy) staging tree to production" )
    <*> flag False True (
            long "pull"
            <> help "Pull (copy) staging tree from production" )
    <*> flag False True (
            long "delete"
            <> short 'D'
            <> help "Remove files from production" )
    <*> flag False True (
            long "symlink"
            <> short 'S'
            <> help "Symlink files in production to staging" )
    <*> many (argument str (metavar "DIRS..."))

data Action = Status | Push | Pull | Delete | Link deriving(Show)

getAction :: CmdLine -> Either String Action
getAction cl = getAction' (clStatus cl) (clPush cl) (clPull cl)
                (clDelete cl) (clSymlink cl)
    where   getAction' True  False False False False = Right Status
            getAction' False True  False False False = Right Push
            getAction' False False True  False False = Right Pull
            getAction' False False False True  False = Right Delete
            getAction' False False False False True  = Right Link
            getAction' False False False False False = Right Status -- Default
            getAction' _ _ _ _ _ = Left "Ambiguous action specified."

main :: IO ()
main = do
    -- Scan the command line
    cl <- execParser opts
    let act_m = getAction cl
    act <- case act_m of
                Left err -> do
                    hPutStrLn stderr $ "Error: " ++ err
                    exitWith $ ExitFailure 1
                Right a -> return a
    
    -- Process each provided dir
    forM_ (clDirs cl) (\ds -> do
        putStrLn $ "Processing: " ++ ds
        d <- encodeUtf ds
        tr <- getDirTree d
        case act of
            Status -> status d tr
            Push -> push d tr
            Pull -> pull d tr
            Delete -> delete d tr
            Link -> symlink d tr
        )
    where
        opts = info (helper <*> cmdLineParser)
            ( fullDesc
                <> progDesc ("Minimal alternative to the \"stow\"; utility.  "
                    ++ "This version is able to copy files instead of "
                    ++ "creating symbolic links.  It therefore also works "
                    ++ "when symlinks are unavailable or unsuitable.")
                <> header "altstow - minimal alternative to stow")
