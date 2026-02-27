{-| FsOps for pure testing.

    The 'FakeFsOps' monad implements FsOps that are not executed on an
    actual file system, but instead modify a DirTree in memory.  This is useful for testing code based on FsOps.
 -}

module FakeFsOps (
    FakeFsOps(..)
) where

import Control.Monad
import Control.Monad.State.Strict
import Data.List.NonEmpty                       (NonEmpty(..))
import qualified Data.Map.Strict                as M
import qualified System.OsPath                  as P

import AstowMonadT
import Diagnostic
import DirTree
import Fallible
import FsOps
import FileUtils

-- | Fake file system.
--
-- Keeps a `DirTree a` in a State Monad where a is the payload type of
-- files.
newtype FakeFsOps a b = FakeFsOps { runFakeFsOps :: State (DirTree a) b }
    deriving (Functor, Applicative, Monad, MonadState (DirTree a))

-- | Monadic 'getNode'.
--
-- Whis one uses the failure handling in 'AstowMonadT' instead of
-- 'Maybe'; makes it more convenient to use in many contexts.
getNodeM :: Monad m => DirTree a -> [ P.OsPath ] -> AstowMonadT m (DirTree a)
getNodeM tr p =
    case getNode p tr of
        Nothing -> do
            tell1 $ Diagnostic ("Accessing " <> osPathToText (P.joinPath p))
                                (TextPayload "no such node")
                                Error
            abort
        Just n -> return n
                    
instance (Eq a) => FsOps (FakeFsOps a) where
    foFilesHaveSameContent a b = do
        tr <- lift get
        fa <- getFile tr a
        fb <- getFile tr b
        return $ fa == fb
        where   getFile :: DirTree a -> P.OsPath -> AstowMonadT (FakeFsOps a) a
                getFile tr p = do
                    nd <- getNodeM tr (P.splitDirectories p)
                    case nd of
                        File x -> return x
                        _ -> do
                            tell1 $ Diagnostic ("Reading " <> osPathToText p)
                                (TextPayload "not a file") Error
                            abort

    foListDirectory a = do
        tr <- lift get
        n <- getNodeM tr (P.splitDirectories a)
        case n of
            Dir dents -> return $ map fst $ M.toAscList dents
            _ -> do
                tell1 $ Diagnostic ("Listing " <> osPathToText a)
                                    (TextPayload "is not a directory")
                                    Error
                abort

    foDoesDirectoryExist d = do
        dt <- lift get
        return $ case getNode (P.splitDirectories d) dt of
            Just (Dir _) -> True
            _ -> False

    foDoesFileExist f = do
        tr <- lift get
        return $ case getNode (P.splitDirectories f) tr of
            Just (File _) -> True
            _ -> False

    foCreateDirectoryIfMissing parents d = do
        let dWhen = "Creating directory " <> osPathToText d
        tr <- lift get
        case P.splitDirectories d of
            [] -> return ()
            (p:ps) -> do    
                tr' <- modifyDirentWith (\path mde ->
                        let mkDir = return $ Just $ mkLine path $ Dir M.empty
                        in case mde of
                               Just (Dir _) -> return mde
                               Just _ -> do
                                   tell1 $ Diagnostic dWhen
                                       (TextPayload "Can't overwrite non-directory")
                                       Error
                                   abort
                               Nothing -> case (parents, path) of
                                   (True, _) -> mkDir
                                   (False, []) -> mkDir
                                   (False, _) -> do
                                       tell1 $ Diagnostic dWhen
                                           (TextPayload "Parent dir does not exist")
                                           Error
                                       abort
                        ) (p :| ps) tr
                lift $ put tr'

    foCopyFileWithMetadata src dest = do -- OsPath -> OsPath -> AstowMonadT m ()
        let dWhen = "Copy " <> osPathToText src <> " -> " <> osPathToText dest
        tr <- lift get
        fa <- getNodeM tr (P.splitDirectories src)
        pb <- case P.splitDirectories dest of 
                (p:ps) -> return (p :| ps)
                [] -> do 
                    tell1 $ Diagnostic dWhen (TextPayload "Missing dest path")
                                Error
                    abort
        case fa of
            File pa -> do
                tr' <- modifyDirentWith (\prest mb -> do
                            unless (null prest) $ do
                                tell1 $ Diagnostic dWhen
                                    (TextPayload "Destination directory missing")
                                    Error
                                abort
                            case mb of
                                    Nothing -> return $ Just (File pa)
                                    Just (File _) -> return $ Just (File pa)
                                    _ -> do
                                        tell1 $ Diagnostic dWhen
                                            (TextPayload "Destination must be file")
                                            Error
                                        abort
                        ) pb tr
                lift $ put tr'
                return ()
            _ -> do
                tell1 $ Diagnostic dWhen
                        (TextPayload "Can't copy non-file") Error
                abort
         
    foRemoveFile file = do
        let dWhen = "Deleting " <> osPathToText file
        tr <- lift get
        ps <- case (P.splitDirectories file) of
                    (p:ps) -> return (p :| ps)
                    [] -> do
                        tell1 $ Diagnostic dWhen (TextPayload "Can't rm root")
                                    Error
                        abort
        tr' <- modifyDirentWith (\prest mb -> do
                    unless (null prest) $ do
                        tell1 $ Diagnostic dWhen
                                    (TextPayload "Empty path") Error
                        abort
                    case mb of 
                        Just (File _) -> return Nothing
                        Nothing -> do
                            tell1 $ Diagnostic dWhen
                                    (TextPayload "No such file") Error
                            abort
                        _ -> do
                            tell1 $ Diagnostic dWhen
                                    (TextPayload "Is not a file") Error
                            abort
                    ) ps tr
        lift $ put tr'
        return ()

    foCreateFileLink = foCopyFileWithMetadata
