module KissDList (
    KissDList
  , singleton
  , fromList
  , toList
) where

newtype KissDList a = KissDList ([a] -> [a])

instance Semigroup (KissDList a) where
    (KissDList f) <> (KissDList g) = KissDList (f . g)

instance Monoid (KissDList a) where
    mempty = empty

empty :: KissDList a
empty = KissDList id

singleton :: a -> KissDList a
singleton !x = fromList [x]

fromList :: [a] -> KissDList a
fromList !xs = KissDList (\l -> xs ++ l)

toList :: KissDList a -> [a]
toList (KissDList f) = f []
