#!/usr/bin/env stack
{-stack
  script
  --resolver lts-18.6
  --package utf8-string
  --package http-client,http-conduit,http-types
  --package tagsoup
-}

import qualified Gov.Top as Top

main :: IO ()
main = do
  Top.grap
