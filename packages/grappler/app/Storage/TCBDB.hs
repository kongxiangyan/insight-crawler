{-# LANGUAGE OverloadedStrings #-}

module Storage.TCBDB
  ( getLatestItem,
    getTestItem,
    addItem,
    addItems,
    itemToJSON,
    itemsToJSON
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.UTF8       as LBU
import           Data.Tuple.Sequence
import qualified Data.Vector                     as V
import           GHC.Float.RealFracMethods       (floorDoubleInteger)
import           Gov.Top
import qualified TencentCloud.CloudBase.Database as TCBDB
import           TencentCloud.Request.Base
import qualified TencentCloud.Request.CloudBase  as R

govStrategiesCollectionName :: String
govStrategiesCollectionName = "GovStrategies"

{-------------------------------------------------------------------------------------------------------
                                                  按照 ID 获取记录
-------------------------------------------------------------------------------------------------------}

getItemById :: String -> IO Item
getItemById docId = do
  requestSettings <- authorizedGetRequestSettings

  responseBody <- R.get requestSettings {
      path = "/databases/" ++
              govStrategiesCollectionName ++
              "/documents/" ++
              docId
    }

  putStrLn (" - The response body was: " ++ responseBody ++ "\n")

  return emptyItem

testDocId :: String
testDocId = "idoftestdoc"
getTestItem :: IO Item
getTestItem = getItemById testDocId

{-------------------------------------------------------------------------------------------------------
                                                  获取最近一条记录
-------------------------------------------------------------------------------------------------------}

-- 请求结果为 EJSON 格式
--    其中数字字段表示为：`\"publishTime\": {\"$numberDouble\":\"0.0\"}`
--    或者 `\"publishTime\": {\"$numberInt\":\"0\"}`
-- JSON 解析后：`("publishTime",Object (fromList [("$numberDouble",String "0.0")]))`

extractEJsonNumber :: Maybe Object -> Maybe Double
extractEJsonNumber obj@(Just _) =  read <$>
  case parseMaybe (.: "$numberDouble") =<< obj of
    Just a  -> a
    Nothing -> parseMaybe (.: "$numberInt") =<< obj
extractEJsonNumber Nothing            = Nothing

parseLatestItem :: LBU.ByteString -> Maybe (String, String, String, Integer, Integer)
parseLatestItem raw = sequenceT (_id, _title, _url, _publishTime, _grapTime)
  where
    obj = decode raw
    _data = parseMaybe (.: "data") =<< obj
    list = parseMaybe (.: "list") =<< _data
    latest = head . V.toList <$> list

    latestObj = decode . LBU.fromString =<< latest
    _id = parseMaybe (.: "_id") =<< latestObj :: Maybe String
    _title = parseMaybe (.: "title") =<< latestObj :: Maybe String
    _url = parseMaybe (.: "url") =<< latestObj :: Maybe String
    _publishTime = floorDoubleInteger <$> (extractEJsonNumber =<< parseMaybe (.: "publishTime") =<< latestObj)
    _grapTime = floorDoubleInteger <$> (extractEJsonNumber =<< parseMaybe (.: "grapTime") =<< latestObj)

getLatestItem :: IO Item
getLatestItem = do
  requestSettings <- authorizedPostRequestSettings

  responseBody <- R.post requestSettings {
      path = "/databases/" ++
              govStrategiesCollectionName ++
              "/documents:find?limit=1&sort={\"publishTime\":-1,\"grapTime\":-1}",
              -- 按发布时间降序排列，发布时间相同的按照抓取时间降序，取时间最近的一条记录
      requestBody = TCBDB.query "{}"
    }

  putStrLn (" - The response body was: " ++ responseBody ++ "\n")

  let parsedResult = parseLatestItem (LBU.fromString responseBody)

  let res = case parsedResult of
              Just (_id, _title, _url, _publishTime, _grapTime) -> Item (_title, _url, _publishTime, _grapTime)
              Nothing -> emptyItem

  return res

{-------------------------------------------------------------------------------------------------------
                                                  添加多条新纪录
-------------------------------------------------------------------------------------------------------}

itemToJSON :: Item -> String
itemToJSON (Item (title, url, publishTime, grapTime)) =
  LBU.toString . encode $ object ["title" .= title, "url" .= url, "publishTime" .= publishTime, "grapTime" .= grapTime]

itemsToJSON :: [Item] -> String
itemsToJSON items = LBU.toString . encode $ object ["data" .= itemsJSON]
  where
    itemsJSON = V.fromList $ map itemToJSON items

addItems :: [Item] -> IO ()
addItems items = do
  requestSettings <- authorizedPostRequestSettings

  responseBody <- R.post requestSettings {
      path = "/databases/" ++
              govStrategiesCollectionName ++
              "/documents",
      requestBody = LBU.fromString $ itemsToJSON items
    }

  print $ responseBody ++ "\n"

{-------------------------------------------------------------------------------------------------------
                                                  添加一条新纪录
-------------------------------------------------------------------------------------------------------}

addItem :: Item -> IO ()
addItem item = addItems [item]
