module TencentCloud.CloudBase.Database
  ( query, queryWithoutEncode
  ) where

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T

queryWithoutEncode :: A.ToJSON v => v -> A.Value
queryWithoutEncode _query = A.object [T.pack "query" A..= _query]

query :: String -> LBS.ByteString
query = A.encode . queryWithoutEncode
