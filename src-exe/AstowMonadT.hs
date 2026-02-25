module AstowMonadT (
    AstowMonadT
  , runAstowMonadT
  , tell
) where

import Control.Monad.Trans
import qualified Control.Monad.Trans.Writer.Strict  as W

import Fallible
import KissDList                                    (KissDList)
import Diagnostic                                   (Diagnostic)

-- | Transformer stack for Astow.
--
-- Standardized Monad to be used everywhere in astow.  It handles failures via
-- FallibleT and keeps a log of errors or messages via WriterT.
type AstowMonadT m = FallibleT (W.WriterT (KissDList Diagnostic) m)

runAstowMonadT :: Monad m => AstowMonadT m a -> m (Fallible a, KissDList Diagnostic)
runAstowMonadT = W.runWriterT . runFallibleT

-- | Add logging.
--
-- Shorthand for using 
-- 'Control.Monad.Writer.Trans.Writer.Strict.tell'
-- in 'AstowMonadT'.
tell :: Monad m => KissDList Diagnostic -> AstowMonadT m ()
tell msgs = lift $ W.tell msgs


