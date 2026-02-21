module FsOps (
    FsOps(..)
) where

import Data.ByteString          as B 
import System.Directory.OsPath  as D
import System.IO                (IOMode(ReadMode))
import System.OsPath            (OsPath)
import System.File.OsPath       (withBinaryFile)

class FsOps m where
    filesHaveSameContent :: OsPath -> OsPath -> m Bool
    listDirectory :: OsPath -> m [OsPath]
    doesDirectoryExist :: OsPath -> m Bool
    doesFileExist :: OsPath -> m Bool
    createDirectoryIfMissing :: Bool -> OsPath -> m ()
    copyFileWithMetadata :: OsPath -> OsPath -> m ()
    removeFile :: OsPath -> m ()
    createFileLink :: OsPath -> OsPath -> m ()

instance FsOps IO where
    filesHaveSameContent a b = do
        withBinaryFile a ReadMode (\ha -> do
            withBinaryFile b ReadMode (\hb -> do
                checkMatch ha hb
                )
            )
        where   checkMatch ha hb = do
                    let blockSz = 65536
                    ba <- B.hGet ha blockSz
                    bb <- B.hGet hb blockSz
                    if ba /= bb then
                        return False
                    else if ba == B.empty then
                        -- EOF reached without finding a difference
                        return True
                    else
                        -- keep checking
                        checkMatch ha hb
    listDirectory = D.listDirectory
    doesDirectoryExist = D.doesDirectoryExist
    doesFileExist = D.doesFileExist
    createDirectoryIfMissing = D.createDirectoryIfMissing
    copyFileWithMetadata = D.copyFileWithMetadata
    removeFile = D.removeFile
    createFileLink = D.createFileLink
