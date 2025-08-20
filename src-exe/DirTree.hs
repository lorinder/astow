module DirTree (
    DirTree(..)
  , getDirTree
  , modifyAttribsM
  , modifyAttribs
  , walk
  , MergeFun
  , merge
) where

import Control.Monad
import Data.Functor.Identity
import Data.List

import qualified System.Directory.OsPath as D
import System.OsPath ((</>), OsPath)

import FileUtils

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
getDirTree
    :: OsPath           -- ^ root path of the tree
    -> a                -- ^ attribute for nodes.
    -> IO (DirTree a)
getDirTree base = getDirTree' base mempty
    where   getDirTree'
                :: OsPath           -- ^ root of the subtree
                -> OsPath           -- ^ file within the root to create tree for
                -> a                -- ^ attribute to assign
                -> IO (DirTree a)   -- ^ resulting dirtree
            getDirTree' location subdir val' = do
                let bloc = location
                fileNames <- sort <$> D.listDirectory (bloc </> subdir)
                files <- forM fileNames (\fn -> do
                    isDir <- D.doesDirectoryExist $ bloc </> subdir </> fn
                    if isDir then 
                        getDirTree' (location </> subdir) fn val'
                    else
                        return $ File fn val'
                    )
                return $ Dir subdir files

-- | Modify tree attributes.
--
--   For each node in a tree, applies a modifier function taking the
--   node's path and its attribute to compute a new attribute value.
modifyAttribs
    :: (OsPath -> a -> b)
    -> DirTree a
    -> DirTree b
modifyAttribs f orig =
    let Identity r = modifyAttribsM (\p x -> Identity $ f p x) orig
    in  r

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

walk :: (Monad m)
    => OsPath                               -- ^ base path
    -> (OsPath -> a -> m b)                 -- ^ visiting function
    -> DirTree a                            -- ^ tree to walk
    -> m [b]
walk path f (File name attr) = do
    r <- f (path </> name) attr
    return [r]
walk path f (Dir name xs) = do
    l <- mapM (walk (path </> name) f) xs
    return $ concat l

-- | Attribute merger function.
type MergeFun a b c =
    OsPath                                  -- ^ Path being merged
    -> Maybe a                              -- ^ left attribute if any
    -> Maybe b                              -- ^ right attribute if any
    -> Either String c                      -- ^ combined attribute or error

-- | Merge two DirTrees.
merge :: DirTree a                          -- ^ left tree
      -> DirTree b                          -- ^ right tree
      -> MergeFun a b c                     -- ^ attribute merger
      -> Either String (DirTree c)          -- ^ combined tree
merge (Dir _ vl) (Dir _ vr) f = do
    l <- mergeLists mempty vl vr f
    pure $ Dir mempty l
merge _ _ _ = Left "Invalid tree passed to merge function"

--
-- merge helper functions functionality
--

leftf
    :: (OsPath -> Maybe a -> Maybe b -> Either String c)
    -> OsPath
    -> a
    -> Either String c
leftf f path x = f path (Just x) Nothing

rightf
    :: (OsPath -> Maybe a -> Maybe b -> Either String c)
    -> OsPath
    -> b
    -> Either String c
rightf f path y = f path Nothing (Just y)

mergeLists
    :: OsPath                                   -- ^ base path
    -> [DirTree a]                              -- ^ left list
    -> [DirTree b]                              -- ^ right list
    -> (OsPath -> Maybe a -> Maybe b -> Either String c)  -- ^ file attribute merger
    -> Either String [DirTree c]
mergeLists _ [] [] _ = return []
mergeLists _ xs [] f =
    traverse (modifyAttribsM $ leftf f) xs
mergeLists _ [] ys f =
    traverse (modifyAttribsM (rightf f)) ys
mergeLists path (x:xs) (y:ys) f = 
    let nx = rootName x
        ny = rootName y
    in  if nx == ny then do
            m <- mergeNode path x y f
            rest <- mergeLists path xs ys f
            return (m:rest)
        else if nx < ny then do
            ma <- modifyAttribsM (leftf f) x
            rest <- mergeLists path xs (y:ys) f
            return (ma:rest)
        else do
            ma <- modifyAttribsM (rightf f) y
            rest <- mergeLists path (x:xs) ys f
            return (ma:rest)

-- | Merge two nodes with identical names.
mergeNode
    :: OsPath                                   -- ^ base path
    -> DirTree a
    -> DirTree b
    -> (OsPath -> Maybe a -> Maybe b -> Either String c)
    -> Either String (DirTree c)
mergeNode path (File name vl) (File _ vr) f = do
    ma <- f (path </> name) (Just vl) (Just vr)
    pure $ File name ma
mergeNode path (Dir name xs) (Dir _ ys) f = do
    ents <- mergeLists path xs ys f
    pure $ Dir name ents
mergeNode path x _ _ =
    Left $ "Node type mismatch for " ++ osPathToString (path </> rootName x)
