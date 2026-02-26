module FileUtils (
    osPathToString
  , osPathToText
) where

import Data.Maybe               (fromMaybe) 
import qualified System.OsPath  as P
import qualified Data.Text      as T

-- | Convert an OsPath to String.
--
-- Convert to a string for the purpose of displaying error messages.  We
-- assume utf-8 encoding.  Currently the error handling is primitive:
-- any invalid utf-8 will result in "[unrepresentable]" rather than an
-- approximation to be displayed.
osPathToString :: P.OsPath -> String
osPathToString p =
    let m_str = P.decodeUtf p :: Maybe String
    in  fromMaybe "[unrepresentable]" m_str

-- | Convert an OsPath to Text.
--
-- See also 'osPathToString'.
osPathToText :: P.OsPath -> T.Text
osPathToText = T.pack . osPathToString
