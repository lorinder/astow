module DirTree (
-- * Type
    DirTree(..)

-- * Tree construction
  , mkLine
  , readFromFs

-- * Lookup & modification primitives
  , getNode
  , modifyDirentWith

-- * Traversal
  , walkM

-- * Merging
  , mergeRight
  , MergeFun
  , theseMergeM
  , mergeDirentsM
) where

import Control.Monad
import Control.Monad.Identity
import Data.List.NonEmpty                           (NonEmpty(..))
import Data.Maybe
import Data.These                                   (These(..))
import qualified Data.Map.Strict                    as M

import System.OsPath                                ((</>), OsPath)

import AstowMonadT
import Diagnostic
import Fallible
import FsOps
import FileUtils

-- | Directory tree representation.
data DirTree a = File a | Dir (M.Map OsPath (DirTree a))
    deriving(Eq)

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

-- | Create a chain of nested subdirectories.
--
-- Make a DirTree that is a line of the given path (i.e. a sequence of
-- nested subdirectories), with the given DirTree at the endpoint.
-- Create a line of nested subdirectory capped by the given endpoint
mkLine
    :: [ OsPath ]                                   -- ^ path to create
    -> DirTree a                                    -- ^ capping endpoint
    -> DirTree a                                    -- ^ resulting line
mkLine [] end = end
mkLine (p:ps) end = Dir $ M.singleton p (mkLine ps end)

-- helper


-- | Create a DirTree from a file system tree.
--
-- Uses FsOps to read a file system tree and creates a DirTree from it.
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
                (TextPayload "Empty result; path may not exist?")
                Error
            abort

-- ReadFromFs helper.
--
-- This is the worker for readFromFS; it has the escape hatch of being
-- able to return Nothing.  This is useful for directory tree where some
-- paths are neither files nor directory;  we want to ignore those
-- without failing altogether.
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

-- | Retrieve the node at the given path.
--
-- Returns 'Just' the node at the specified path, or 'Nothing' if there
-- is no such node.  The location is given in separate components;
-- 'System.OsPath.splitDirectories' can be used to obtain it from a
-- single string.
getNode
    :: [ OsPath ]                                   -- ^ path
    -> DirTree a                                    -- ^ tree root
    -> Maybe (DirTree a)                            -- ^ retrieved node
getNode [] tr = Just tr
getNode (p:ps) tr =
    case tr of
        Dir dents -> case M.lookup p dents of
            Nothing -> Nothing
            Just tr' -> getNode ps tr'
        _ -> Nothing
    
-- | Modify the containing directory of a path.
--
-- Descends into the tree along the given path, until either
-- 
-- - there is only one path component left, or
-- - an intermediate component does not exist.
--
-- In either case, the directory mapping is then updated with the
-- function which receives remainder path.  If during the descent a file
-- is encountered instead of a directory, this is treated as an error,
-- and the modification is aborted.
--
-- The modifier receives a leftover path, and the previous entry if any.
-- The leftover path is the set of path components that weren't resolved
-- before the end of the tree is used.  It is typically used to abort
-- with an error if it's nonempty, or to create the missing intermediate
-- directories, e.g. with mkLine.
modifyDirentWith :: forall a m. Monad m =>
       ([ OsPath ]                                  -- ^ fun remainder path
        -> Maybe (DirTree a)                        -- ^ original entry
        -> AstowMonadT m (Maybe (DirTree a))        -- ^ updated entry
        )                                           -- ^ modifier function
    -> NonEmpty OsPath                              -- ^ descent path
    -> DirTree a                                    -- ^ tree to descend
    -> AstowMonadT m (DirTree a)                    -- ^ updated tree
modifyDirentWith f (p :| ps) tr =
    case tr of
        File _ -> do
            tell1 $ Diagnostic "node modification"
                        (TextPayload "Path component is not a directory")
                        Error
            abort
        Dir dents -> do
            let mde = M.lookup p dents
            case ps of
                [] -> doUpdate mde dents
                (p':ps') -> case mde of
                    Nothing -> doUpdate mde dents
                    Just subtr -> do
                        subtr' <- modifyDirentWith f (p' :| ps') subtr
                        return $ Dir (M.insert p subtr' dents)
    where   -- doUpdate computes the update on the final node by wrapping f.
            doUpdate :: Monad m
                => Maybe (DirTree a)
                -> M.Map OsPath (DirTree a)
                -> AstowMonadT m (DirTree a)
            doUpdate mde dents = do
                mde' <- f ps mde
                let dents' = case mde' of
                        Nothing -> M.delete p dents
                        Just de -> M.insert p de dents
                return $ Dir dents'

-- | Visit all the nodes in a directory tree.
--
--   Each node is visited by a monadic visitor function which as
--   arguments receives the path of the visitor function and the
--   attribute stored in the node.
--
--   Returned is a linearized list of the visit results.
walkM :: (Monad m)
    => OsPath                               -- ^ base path
    -> (OsPath -> a -> m b)                 -- ^ visiting function
    -> DirTree a                            -- ^ tree to walk
    -> m [b]
walkM path f (File attr) = (:[]) <$> f path attr
walkM path f (Dir entsMap) = do
    let ents = M.toAscList entsMap
    concat <$> forM ents (\(fn, item) -> walkM (path </> fn) f item)

-- | Merge a DirTree with the right hand side breaking ties.
--
-- The left and right DirTree need to have the same payload type.  Files
-- and directories appearing in only one tree appear unchanged in the
-- merged trees. Directories appearing in both have their entries
-- recursively merged with the same rule.  Other nodes that appear in
-- both trees take the node type and payload types of the right.  This
-- is in particular true, if a node appears in both trees but is of a
-- different type.
mergeRight
    :: DirTree a        -- ^ left tree
    -> DirTree a        -- ^ right tree
    -> DirTree a        -- ^ merged tree
mergeRight l r = runIdentity $ mergeRightM mempty (These l r)
    where   mergeRightM :: Monad m
                => OsPath
                -> These (DirTree a) (DirTree a)
                -> m (DirTree a)
            mergeRightM _ (This x) = return x
            mergeRightM _ (That y) = return y
            mergeRightM p (These x y) = case (x, y) of
                (Dir dx, Dir dy) -> Dir <$> mergeDirentsM mergeRightM p dx dy
                (_, fy) -> return fy

-- | Node merging function.
--
--   This is the custom node merging function that is supplied the
--   merger.  The nodes passed are either individual nodes, when the
--   node of one name only appears on either side.  Or they can be both
--   Files, refering to corresponding File nodes in the trees to be
--   merged. In special cases, one of them might be a directory and
--   another one a file.  That case is an error in many uses of merging,
--   and could be signaled through the monad m for example. 
--
type MergeFun m a b c =
    OsPath                                  -- ^ Path being merged
    -> These (DirTree a) (DirTree b)        -- ^ Nodes to merge
    -> m (DirTree c)                        -- ^ combined attribute or error

-- | Merge function that holds the result as a 'These'.
--
-- Each file node has the result stored as a 'These' a b.  Therefore,
-- using a mapping over the functor of the related tree, any desired
-- combination of the arguments can be implemented.
--
-- It is an error to try to merge incompatible node types with this
-- function.
theseMergeM :: Monad m
    => OsPath
    -> These (DirTree a) (DirTree b)
    -> AstowMonadT m (DirTree (These a b))
theseMergeM p (This x) = case x of
    File fx -> return $ File (This fx)
    Dir dx -> Dir <$> mergeDirentsM theseMergeM p dx M.empty
theseMergeM p (That y) = case y of
    File fy -> return $ File (That fy)
    Dir dy -> Dir <$> mergeDirentsM theseMergeM p M.empty dy
theseMergeM p (These x y) = case (x, y) of
    (File fx, File fy) -> return $ File (These fx fy)
    (Dir dx, Dir dy) -> Dir <$> mergeDirentsM theseMergeM p dx dy
    _ -> do
        tell1 $ Diagnostic ("merge " <> osPathToText p)
                    (TextPayload "Incompatible node type") Error
        abort

-- | Merge two directory listings.
mergeDirentsM :: forall m a b c. Monad m
    => MergeFun m a b c
    -> OsPath
    -> M.Map OsPath (DirTree a)
    -> M.Map OsPath (DirTree b)
    -> m (M.Map OsPath (DirTree c))
mergeDirentsM mergeFn path lmap rmap =
    let lsOrig = M.toAscList lmap
        rsOrig = M.toAscList rmap
        mergeLists
            :: [(OsPath, DirTree a)]
            -> [(OsPath, DirTree b)]
            -> m [(OsPath, DirTree c)]
        mergeLists ls@((lname,lx):ls') rs@((rname,rx):rs') =
            case compare lname rname of
                LT -> do
                    l <- mergeL lname lx
                    (l:) <$> mergeLists ls' rs
                GT -> do
                    r <- mergeR rname rx
                    (r:) <$> mergeLists ls rs'
                EQ -> do
                    merged <- mergeFn (path </> lname) (These lx rx)
                    ((lname,merged):) <$> mergeLists ls' rs'
        mergeLists ((lname,lx):ls') [] = do
            l <- mergeL lname lx
            (l:) <$> mergeLists ls' []
        mergeLists [] ((rname,rx):rs') = do
            r <- mergeR rname rx
            (r:) <$> mergeLists [] rs'
        mergeLists [] [] = return []
        mergeL :: OsPath -> DirTree a -> m (OsPath, DirTree c)
        mergeL lname lx@(File _) = do
            x <- mergeFn (path </> lname) (This lx)
            return (lname, x)
        mergeL lname (Dir lents) = do
            x <- mergeDirentsM mergeFn (path </> lname) lents M.empty
            return (lname, Dir x)
        mergeR :: OsPath -> DirTree b -> m (OsPath, DirTree c)
        mergeR rname rx@(File _) = do
            x <- mergeFn (path </> rname) (That rx)
            return (rname, x)
        mergeR rname (Dir rents) = do
            x <- mergeDirentsM mergeFn (path </> rname) M.empty rents
            return (rname, Dir x)
    in  do
            lst <- mergeLists lsOrig rsOrig
            return $ M.fromAscList lst
