module Diagnostic (
    Diagnostic(..)
  , Payload(..)
  , Severity(..)

-- * APIs
  , diagnosticMessage
) where

import Control.Exception
import System.IO.Error

data Diagnostic = Diagnostic {
        diagWhen        :: String       -- ^ When did the problem happen ?
      , diagPayload     :: Payload      -- ^ Payload (e.g. exception)
      , diagSeverity    :: Severity     -- ^ severity
    }

-- | Error payload, e.g. exception.
data Payload =
        NoPayload                       -- ^ No further information
      | IOPayload IOException           -- ^ When an IO error occurred

-- | Severity of a diagnostic.
data Severity = Info | Warning | Error
    deriving (Eq, Ord, Show)

-- | Create a message from a diagnostic.
diagnosticMessage :: Diagnostic -> String
diagnosticMessage d =
    let severityLabel = show $ diagSeverity d
    in  severityLabel ++ ": " ++ (diagWhen d) ++
            case diagPayload d of
                NoPayload -> ""
                IOPayload e -> ": " ++ ioeGetErrorString e
