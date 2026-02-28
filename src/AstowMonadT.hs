module AstowMonadT (
    AstowMonadT(..)
  , tell
  , tell1
) where

import Control.Monad.Trans
import qualified Control.Monad.Trans.Writer.Strict  as W

import Fallible
import KissDList                                    (KissDList, singleton)
import Diagnostic                                   (Diagnostic)

-- | Transformer stack for Astow.
--
-- Standardized Monad to be used everywhere in astow.  It handles failures via
-- FallibleT and keeps a log of errors or messages via WriterT.
newtype AstowMonadT m a = AstowMonadT {
        runAstowMonadT :: FallibleT (W.WriterT (KissDList Diagnostic) m) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadFallible)

instance MonadTrans AstowMonadT where
    lift :: Monad m => m a -> AstowMonadT m a
    lift = AstowMonadT . lift . lift

-- | Log diagnostics.
--
-- Shorthand for using 
-- 'Control.Monad.Writer.Trans.Writer.Strict.tell'
-- in 'AstowMonadT'.
tell :: Monad m => KissDList Diagnostic -> AstowMonadT m ()
tell msgs = AstowMonadT $ lift $ W.tell msgs

-- | Log a single diagnostic message.
--
-- Simple wrapper around 'tell' to log just one message.
tell1 :: Monad m => Diagnostic -> AstowMonadT m ()
tell1 = tell . singleton
