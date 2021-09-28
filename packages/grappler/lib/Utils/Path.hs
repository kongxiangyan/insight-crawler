module Utils.Path
  ( appDataDir,
    pathInAppDataDir
  ) where

import           Data.Functor
import           System.Directory
import           System.FilePath

appDataDir :: IO FilePath
appDataDir = do
  dir <- getAppUserDataDirectory "insights-grappler"
  createDirectoryIfMissing True dir
  return dir

pathInAppDataDir :: FilePath -> IO FilePath
pathInAppDataDir path =  appDataDir <&> flip combine path
