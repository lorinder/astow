module DirTree (
    DirTree(..)
  , getDirTree
  , modifyAttribsM
  , walkM

-- * Merging
  , MergeFun
  , LeftRightAttribs(..)
  , merge
) where

import Control.Monad
import Data.List (sort)

import System.OsPath ((</>), OsPath)

import AstowMonadT
import Diagnostic
import Fallible
import FsOps
import FileUtils
import KissDList (singleton)

data DirTree a = File OsPath a | Dir OsPath [DirTree a]

deriving instance Show a => Show (DirTree a)

rootName :: DirTree a -> OsPath
rootName (File x _) = x
rootName (Dir x _) = x

-- | Standard typeclasses for DirTree.

instance Functor DirTree where
    fmap :: (a -> b) -> DirTree a -> DirTree b
    fmap f (File name x) = File name (f x)
    fmap f (Dir name xs) = Dir name (map (fmap f) xs)

instance Foldable DirTree where
    foldr :: (a -> b -> b) -> b -> DirTree a -> b
    foldr f acc x = foldr f acc $ flattenAttribs x

flattenAttribs :: DirTree a -> [a]
flattenAttribs (File _ x) = [x]
flattenAttribs (Dir _ xs) = concatMap flattenAttribs xs

-- IO functions

-- | Create a DirTree from a file system tree.
--
-- Walks a real file system tree and creates a DirTree from it,
-- assigning a fixed attribute to each node.
getDirTree :: forall a m. (Monad m, FsOps m)
    => OsPath           -- ^ root path of the tree
    -> a                -- ^ attribute for nodes
    -> AstowMonadT m (DirTree a)
getDirTree base = getDirTree' base mempty
    where   getDirTree'
                :: OsPath           -- ^ root of the subtree
                -> OsPath           -- ^ file within the root to create tree for
                -> a                -- ^ attribute to assign
                -> AstowMonadT m (DirTree a) -- ^ resulting dirtree
            getDirTree' location subdir val' = do
                let bloc = location
                fileNames <- sort <$> foListDirectory (bloc </> subdir)
                files <- forM fileNames (\fn -> do
                    isDir <- foDoesDirectoryExist $ bloc </> subdir </> fn
                    if isDir then 
                        getDirTree' (location </> subdir) fn val'
                    else
                        return $ File fn val'
                    )
                return $ Dir subdir files

-- | Modify tree attributes, monadic version.
--
--   'modifyAttribs' with a monadic modifier function.
modifyAttribsM :: (Monad m)
    => (OsPath -> a -> m b)                 -- ^ modifier function
    -> DirTree a                            -- ^ the tree to modify
    -> m (DirTree b)                        -- ^ resulting output
modifyAttribsM = modifyAttribsM' mempty
    where   modifyAttribsM' :: (Monad m)
                => OsPath                   -- ^ root path
                -> (OsPath -> a -> m b)     -- ^ modifier func
                -> DirTree a                -- ^ the tree to take
                -> m (DirTree b)
            modifyAttribsM' path f (File name attr) = do
                attr' <- f (path </> name) attr
                return $ File name attr'
            modifyAttribsM' path f (Dir name xs) = do
                let path' = path </> name
                let mxs = map (modifyAttribsM' path' f) xs
                l <- sequence mxs
                return $ Dir name l

-- | Visit all the nodes in a directory tree.
--
--   Each node is visited by a monadic visitor function which as
--   arguments receives the path of the visitor function and the
--   attribute stored in the node.
--
--   Returned is a linearlized list of the visit results.
walkM :: (Monad m)
    => OsPath                               -- ^ base path
    -> (OsPath -> a -> m b)                 -- ^ visiting function
    -> DirTree a                            -- ^ tree to walk
    -> m [b]
walkM path f (File name attr) = do
    r <- f (path </> name) attr
    return [r]
walkM path f (Dir name xs) = do
    l <- mapM (walkM (path </> name) f) xs
    return $ concat l

-- | Node attributes for merging trees.
--
-- Merging of a file node means that it must be present in either the
-- left tree, the right tree, or both.
data LeftRightAttribs a b
    = LeftOnly a
      | RightOnly b
      | Both a b

-- | Attribute merger function.
type MergeFun m a b c =
    OsPath                                  -- ^ Path being merged
    -> LeftRightAttribs a b                 -- ^ Attributes
    -> AstowMonadT m c                      -- ^ combined attribute or error

-- | Merge two DirTrees.
merge :: (Monad m)
      => DirTree a                          -- ^ left tree
      -> DirTree b                          -- ^ right tree
      -> MergeFun m a b c                   -- ^ attribute merger
      -> AstowMonadT m (DirTree c)          -- ^ combined tree
merge (Dir _ vl) (Dir _ vr) f = do
    l <- mergeLists mempty vl vr f
    pure $ Dir mempty l
merge _ _ _ = do
    tell $ singleton (Diagnostic "tree-merge"
            (TextPayload "tree roots need to be directories")
            Error)
    abort

--
-- merge helper functions functionality
--

mergeLists
    :: Monad m
    => OsPath                               -- ^ base path
    -> [DirTree a]                          -- ^ left list
    -> [DirTree b]                          -- ^ right list
    -> MergeFun m a b c                     -- ^ file attribute merger
    -> AstowMonadT m [DirTree c]
mergeLists _ [] [] _ = return []
mergeLists _ xs [] f =
    traverse (modifyAttribsM $ \p x' -> f p (LeftOnly x')) xs
mergeLists _ [] ys f =
    traverse (modifyAttribsM $ \p y' -> f p (RightOnly y')) ys
mergeLists path (x:xs) (y:ys) f = 
    let nx = rootName x
        ny = rootName y
    in  if nx == ny then do
            m <- mergeNode path x y f
            rest <- mergeLists path xs ys f
            return (m:rest)
        else if nx < ny then do
            ma <- modifyAttribsM (\p x' -> f p (LeftOnly x')) x
            rest <- mergeLists path xs (y:ys) f
            return (ma:rest)
        else do
            ma <- modifyAttribsM (\p y' -> f p (RightOnly y')) y
            rest <- mergeLists path (x:xs) ys f
            return (ma:rest)

-- | Merge two nodes with identical names.
mergeNode
    :: Monad m
    => OsPath                                   -- ^ base path
    -> DirTree a
    -> DirTree b
    -> MergeFun m a b c
    -> AstowMonadT m (DirTree c)
mergeNode path (File name vl) (File _ vr) f = do
    ma <- f (path </> name) (Both vl vr)
    pure $ File name ma
mergeNode path (Dir name xs) (Dir _ ys) f = do
    ents <- mergeLists path xs ys f
    pure $ Dir name ents
mergeNode path (File name _) _ _ = mergeInconsistency path name
mergeNode path _ (File name _) _ = mergeInconsistency path name

-- | Deal with a node type mismatch.
--
-- We log the error, and mark the result as invalid.  We want to avoid
-- to abort, since it is useful to check for further errors.  In order
-- to achieve this, we merge the node as a dummy directory node; this
-- avoid having to invoke the merge function.
mergeInconsistency :: Monad m => OsPath -> OsPath -> AstowMonadT m (DirTree c)
mergeInconsistency path name = do
    tell $ singleton (Diagnostic "tree-merge"
                (TextPayload $ "Node type mismatch for "
                    <> osPathToText (path </> name))
                Error)
    invalid $ Dir name []
