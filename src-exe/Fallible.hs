module Fallible (
-- * Monad
    Fallible(..)
  , fallible

-- * Transformer
  , FallibleT(..)

-- * Utilities
  , MonadFallible(..)
) where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

-- | Fallible computation value
--
-- Can be one of
--
-- * Aborted (without value)
-- * Completed False x (completed, but with errors; result not fully
--   valid)
-- * Completed True x (completed successfully.)
data Fallible a =
      Aborted               -- ^ Computation aborted without result.
    | Completed             -- ^ Computation completed
        !Bool               -- ^ Result validity flag
        a                   -- ^ Result value

-- | Convert a fallible value.
--
-- Function modeled after the 'maybe' or 'either' functions from the
-- prelude.
--
-- Example:  To convert a 'Fallible' value into a 'Maybe' value with
-- only the valid results containing a value, use the function
--
-- >>>> fallible Nothing (const Nothing) Just
fallible
    :: b                    -- ^ Output for Aborted computation
    -> (a -> b)             -- ^ Compute output from invalid value
    -> (a -> b)             -- ^ Compute output from valid value
    -> Fallible a           -- ^ Input fallible
    -> b                    -- ^ Resulting output
fallible v_abort f_invalid f_valid x =
    case x of
        Aborted -> v_abort
        Completed False x' -> f_invalid x'
        Completed True x' -> f_valid x'

instance (Show a) => Show (Fallible a) where
    show x = case x of
        Aborted -> "Aborted"
        Completed c v -> "Completed " ++ show c ++ " " ++ show v

instance Functor Fallible where
    fmap f x = case x of
        Aborted -> Aborted
        Completed valid x' -> Completed valid (f x')

instance Applicative Fallible where
    pure x = Completed True x
    liftA2 f x y =
        case x of
            Aborted -> Aborted
            Completed valid_x x' -> case y of
                Aborted -> Aborted
                Completed valid_y y' ->
                    Completed (valid_x && valid_y) (f x' y')

instance Monad Fallible where
    x >>= f = case x of
        Aborted -> Aborted
        Completed (valid_x) x' -> case (f x') of
            Aborted -> Aborted
            Completed (valid_y) y ->
                Completed (valid_x && valid_y) y

-- The transformer

newtype FallibleT m a = FallibleT { runFallibleT :: m (Fallible a) }

instance (Functor m) => Functor (FallibleT m) where
    fmap f x = FallibleT $ fmap (fmap f) (runFallibleT x)

instance (Applicative m) => Applicative (FallibleT m) where
    pure x = FallibleT $ pure (pure x)
    liftA2 f x y = FallibleT $
                    liftA2 (liftA2 f) (runFallibleT x) (runFallibleT y)

instance (Monad m) => Monad (FallibleT m) where
    x >>= f =
        let xInner = runFallibleT x
            fInner = runFallibleT . f
        in  FallibleT $ do
                x' <- xInner
                case x' of
                    Aborted -> return Aborted
                    Completed valid_x x'' -> do
                        y <- fInner x''
                        return $ case y of
                            Aborted -> Aborted
                            Completed valid_y y'' ->
                                Completed (valid_x && valid_y) y''
-- Utility funcs

class MonadFallible m where
    abort :: m a
    invalid :: a -> m a

instance MonadFallible Fallible where
    abort = Aborted
    invalid = Completed False

instance (Monad m) => MonadFallible (FallibleT m) where
    abort = FallibleT (return $ Aborted)
    invalid x = FallibleT (return $ Completed False x)

-- Monad transformer.

instance MonadTrans FallibleT where
    lift :: Monad m => m a -> FallibleT m a
    lift f = FallibleT $ Completed True <$> f

instance (MonadIO m) => MonadIO (FallibleT m) where
    liftIO :: IO a -> FallibleT m a
    liftIO f = FallibleT $ Completed True <$> (liftIO f)
