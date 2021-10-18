{-# LANGUAGE OverloadedStrings #-}

module TencentCloud.CloudBase.Database
  ( query, queryWithoutEncode
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.Text            as T

queryWithoutEncode :: ToJSON v => v -> Value
queryWithoutEncode _query = object ["query" .= _query]

query :: String -> LBS.ByteString
query = encode . queryWithoutEncode
