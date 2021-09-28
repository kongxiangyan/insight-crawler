module Utils.Time
  ( generalFormatTime,
    generalFormatDate,
    getTimestamp
  ) where

import           Data.Time
import           Data.Time.Clock.POSIX

generalFormatTime :: FormatTime t => t -> String
generalFormatTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

generalFormatDate :: FormatTime t => t -> String
generalFormatDate = formatTime defaultTimeLocale "%Y-%m-%d"

getTimestamp :: UTCTime -> Integer
getTimestamp = floor . utcTimeToPOSIXSeconds
