module Diagnostic (
    Diagnostic(..)
  , Payload(..)
  , Severity(..)

-- * APIs
  , diagnosticMessage
  , mkInfoDiagnostic
) where

import Control.Exception
import System.IO.Error
import qualified Data.Text              as T

data Diagnostic = Diagnostic {
        diagWhen        :: !T.Text       -- ^ When did the problem happen ?
      , diagPayload     :: !Payload      -- ^ Payload (e.g. exception)
      , diagSeverity    :: !Severity     -- ^ severity
    }

-- | Error payload, e.g. exception.
data Payload =
        NoPayload                       -- ^ No further information
      | IOPayload IOException           -- ^ When an IO error occurred
      | TextPayload T.Text              -- ^ Textual error message

-- | Severity of a diagnostic.
data Severity = Info | Warning | Error
    deriving (Eq, Ord)

-- | Create a message from a diagnostic.
diagnosticMessage :: Diagnostic -> T.Text
diagnosticMessage d =
    let severityLabel = case diagSeverity d of
            Info -> "Info"
            Warning -> "Warning"
            Error -> "Error"
    in  severityLabel <> ": " <> (diagWhen d) <>
            case diagPayload d of
                NoPayload -> ""
                IOPayload e -> ": " <> T.pack (ioeGetErrorString e)
                TextPayload t -> ": " <> t

mkInfoDiagnostic :: T.Text -> Diagnostic
mkInfoDiagnostic !msg = Diagnostic msg NoPayload Info
