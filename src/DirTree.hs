module DirTree (
-- * Type
    DirTree(..)

-- * Creation from FsOps (e.g. file system)
  , readFromFs

-- * Traversal
  , walkM

-- * Merging
  , MergeFun
  , LeftRightAttribs(..)
  , merge
) where

import Control.Monad
import Data.Maybe
import qualified Data.Map.Strict                    as M

import System.OsPath                                ((</>), OsPath)

import AstowMonadT
import Diagnostic
import Fallible
import FsOps
import FileUtils

-- | Directory tree representation.
data DirTree a = File a | Dir (M.Map OsPath (DirTree a))

-- display for debugging
instance Show a => Show (DirTree a) where
    show = showWithDepth 0

-- | Display with a given indent.
--
-- It writes it out in multiple lines with indents that make skimming
-- the directory tree easy, but otherwise does preserve (apart from the
-- fact that OsPath is not reversible in that sense.)
showWithDepth :: Show a => Int -> DirTree a -> String
showWithDepth n tr = 
    let sw = 4
    in  case tr of
            File x -> "File " ++ show x
            Dir dents ->
                let ndents = M.size dents
                    dents' = M.toAscList dents
                in  "Dir (fromList [" ++
                    if ndents == 0
                        then "])\n"
                        else concatMap (\(i, (name, subtr) ) ->
                            "\n" ++ (take (sw * n) $ repeat ' ')
                            ++ "(" ++ show name ++ ", "
                            ++ (showWithDepth (n + 1) subtr)
                            ++ ")"
                            ++ if i < ndents - 1 then "," else "])")
                                $ zip [0..] dents'

instance Functor DirTree where
    fmap :: (a -> b) -> DirTree a -> DirTree b
    fmap f (File x) = File (f x)
    fmap f (Dir ents) = Dir (M.map (fmap f) ents) 


maybeReadFromFs :: forall m. (Monad m, FsOps m)
    => OsPath           -- ^ base (root) path
    -> AstowMonadT m (Maybe (DirTree ()))
maybeReadFromFs base = do
    isDir <- foDoesDirectoryExist base
    if isDir then do
        fileNames <- foListDirectory base
        msubtrees <- forM fileNames (\fn -> do
            msubtr <- maybeReadFromFs (base </> fn)
            return (fn, msubtr))
        let subtrees = mapMaybe (\(fn, msubtr) ->
                case msubtr of
                    Just subtr -> Just (fn, subtr)
                    Nothing -> Nothing) msubtrees
        return $ Just $ Dir (M.fromList subtrees)
    else do
        isFile <- foDoesFileExist base
        return $ if isFile then (Just $ File ()) else Nothing


-- | Create a DirTree from a file system tree.
--
-- Walks a real file system tree and creates a DirTree from it,
-- assigning a fixed attribute to each node.
readFromFs :: forall m. (Monad m, FsOps m)
    => OsPath           -- ^ root to start scanning from
    -> AstowMonadT m (DirTree ())
readFromFs base = do
    mtr <- maybeReadFromFs base
    case mtr of
        Just tr -> return tr
        Nothing -> do
            tell1 $ Diagnostic
                ("Reading directory " <> osPathToText base)
                NoPayload
                Error
            abort

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
walkM path f (File attr) = (:[]) <$> f path attr
walkM path f (Dir entsMap) = do
    let ents = M.toAscList entsMap
    concat <$> forM ents (\(fn, item) -> walkM (path </> fn) f item)

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
      => MergeFun m a b c                   -- ^ attribute merger
      -> OsPath                             -- ^ root path to use
      -> DirTree a                          -- ^ left tree
      -> DirTree b                          -- ^ right tree
      -> AstowMonadT m (DirTree c)          -- ^ combined tree
merge f path (File l) (File r) = File <$> f path (Both l r)
merge f path (Dir ls) (Dir rs) = do
    Dir <$> mergeDirents f path ls rs
merge _ path _ _ = do
    tell1 $ Diagnostic ("Attempting to merge " <> osPathToText path)
                (TextPayload $ "Node type mismatch")
                Error
    invalid $ Dir M.empty

--
-- merge helper functions functionality
--

mergeDirents :: forall m a b c. Monad m
    => MergeFun m a b c
    -> OsPath
    -> M.Map OsPath (DirTree a)
    -> M.Map OsPath (DirTree b)
    -> AstowMonadT m (M.Map OsPath (DirTree c))
mergeDirents f path lmap rmap =
    let lsOrig = M.toAscList lmap
        rsOrig = M.toAscList rmap
        mergeLists
            :: [(OsPath, DirTree a)]
            -> [(OsPath, DirTree b)]
            -> AstowMonadT m [(OsPath, DirTree c)]
        mergeLists ls@((lname,lx):ls') rs@((rname,rx):rs') =
            case compare lname rname of
                LT -> do
                    l <- mergeLeft lname lx
                    (l:) <$> mergeLists ls' rs
                GT -> do
                    r <- mergeRight rname rx
                    (r:) <$> mergeLists ls rs'
                EQ -> do
                    merged <- merge f (path </> lname) lx rx
                    ((lname,merged):) <$> mergeLists ls' rs'
        mergeLists ((lname,lx):ls') [] = do
            l <- mergeLeft lname lx
            (l:) <$> mergeLists ls' []
        mergeLists [] ((rname,rx):rs') = do
            r <- mergeRight rname rx
            (r:) <$> mergeLists [] rs'
        mergeLists [] [] = return []
        mergeLeft :: OsPath -> DirTree a -> AstowMonadT m (OsPath, DirTree c)
        mergeLeft lname (File lx) = do
            x <- f (path </> lname) (LeftOnly lx)
            return (lname, File x)
        mergeLeft lname (Dir lents) = do
            x <- mergeDirents f (path </> lname) lents M.empty
            return (lname, Dir x)
        mergeRight :: OsPath -> DirTree b -> AstowMonadT m (OsPath, DirTree c)
        mergeRight rname (File rx) = do
            x <- f (path </> rname) (RightOnly rx)
            return (rname, File x)
        mergeRight rname (Dir rents) = do
            x <- mergeDirents f (path </> rname) M.empty rents
            return (rname, Dir x)
    in  do
            lst <- mergeLists lsOrig rsOrig
            return $ M.fromAscList lst
