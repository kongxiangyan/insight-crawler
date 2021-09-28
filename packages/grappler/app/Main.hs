#!/usr/bin/env stack
{-stack
  script
  --resolver lts-18.6
  --package time
  --package memory
  --package utf8-string,bytestring,directory,filepath
  --package http-client,http-client-tls,http-conduit,http-types
  --package tagsoup
  --package cryptonite
  --package aeson
-}

import           Control.Concurrent
import           Control.Monad
import qualified Gov.Top            as Top
import qualified Storage.TCBDB      as DB

-- main :: IO ()
main = do
  DB.getTestItem
  DB.getLatestItem
-- main = forever $
--   forkIO (do
--     grappledData <- Top.grap
--     latestItem <- DB.getLatestItem
--     return ()
--   )
--   >> threadDelay (60 * 10 * 1000000)
