module Utils.Time
  ( generalFormatTime,
    generalFormatDate,
    parseGeneralFormatTime,
    parseGeneralFormatDate,
    getTimestamp,
    timestampToUTC
  ) where

import           Data.Time
import           Data.Time.Clock.POSIX

generalFormatTime :: FormatTime t => t -> String
generalFormatTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

generalFormatDate :: FormatTime t => t -> String
generalFormatDate = formatTime defaultTimeLocale "%Y-%m-%d"

parseGeneralFormatTime :: String -> Maybe UTCTime
parseGeneralFormatTime = parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S"

parseGeneralFormatDate :: String -> Maybe UTCTime
parseGeneralFormatDate = parseTimeM False defaultTimeLocale "%Y-%m-%d"

getTimestamp :: UTCTime -> Integer
getTimestamp = floor . utcTimeToPOSIXSeconds

timestampToUTC :: Integer -> UTCTime
timestampToUTC = posixSecondsToUTCTime . realToFrac
