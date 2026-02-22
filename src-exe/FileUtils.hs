module FileUtils (
    osPathToString
) where

import Data.Maybe               (fromMaybe) 
import System.OsPath            as P

-- | Convert an OsPath to a string.
--
-- Convert to a string for the purpose of displaying error messages.  We
-- assume utf-8 encoding.  Currently the error handling is primitive:
-- any invalid utf-8 will result in "[unrepresentable]" rather than an
-- approximation to be displayed.
osPathToString
    :: OsPath
    -> String
osPathToString p =
    let m_str = decodeUtf p :: Maybe String
    in  fromMaybe "[unrepresentable]" m_str
