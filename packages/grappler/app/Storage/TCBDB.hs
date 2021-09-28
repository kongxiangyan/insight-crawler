{-# LANGUAGE OverloadedStrings #-}

module Storage.TCBDB
  ( getLatestItem,
    getTestItem
  ) where

-- import qualified Data.Aeson                     as A
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy.UTF8       as BU
import           Gov.Top
import           System.Environment
import qualified TencentCloud.CloudBase.Database as TCBDB
import           TencentCloud.Request.Base
import qualified TencentCloud.Request.CloudBase  as R

govStrategiesCollectionName :: String
govStrategiesCollectionName = "GovStrategies"

emptyItem :: Item
emptyItem = Item ("", "", "", "")

{-------------------------------------------------------------------------------------------------------
                                                  按照 ID 获取记录
-------------------------------------------------------------------------------------------------------}

getItemById :: String -> IO Item
getItemById docId = do
  -- 从环境变量中获取云服务资源操作密钥
  _secretId <- getEnv "TC_SECRET_ID"
  _secretKey <- getEnv "TC_SECRET_KEY"
  _envId <- getEnv "TCB_ENV_ID"

  responseBody <- R.get getRequestSettings {
      secretId = _secretId,
      secretKey = _secretKey,
      envId = _envId,
      path = "/databases/" ++
              govStrategiesCollectionName ++
              "/documents/" ++
              docId
    }

  putStrLn responseBody

  return emptyItem

testDocId :: String
testDocId = "idoftestdoc"
getTestItem :: IO Item
getTestItem = getItemById testDocId

{-------------------------------------------------------------------------------------------------------
                                                  获取最近一条记录
-------------------------------------------------------------------------------------------------------}

getLatestItem :: IO Item
getLatestItem = do
  -- 从环境变量中获取云服务资源操作密钥
  _secretId <- getEnv "TC_SECRET_ID"
  _secretKey <- getEnv "TC_SECRET_KEY"
  _envId <- getEnv "TCB_ENV_ID"

  res <- R.post postRequestSettings {
      secretId = _secretId,
      secretKey = _secretKey,
      envId = _envId,
      path = "/databases/" ++
              govStrategiesCollectionName ++
              "/documents:find?limit=1&sort={\"publishTimestamp\":-1}",
              -- 按发布时间降序排列，取发布时间的时间戳最大的一条记录
      requestBody = TCBDB.query "{}"
    }

  putStrLn res

  return emptyItem

{-------------------------------------------------------------------------------------------------------
                                                  添加一条新纪录
-------------------------------------------------------------------------------------------------------}

addItem :: IO ()
addItem = do
  -- 从环境变量中获取云服务资源操作密钥
  _secretId <- getEnv "TC_SECRET_ID"
  _secretKey <- getEnv "TC_SECRET_KEY"
  _envId <- getEnv "TCB_ENV_ID"

  res <- R.post postRequestSettings {
      secretId = _secretId,
      secretKey = _secretKey,
      envId = _envId,
      path = "/databases/"
    }

  putStrLn res

{-------------------------------------------------------------------------------------------------------
                                                  添加多条新纪录
-------------------------------------------------------------------------------------------------------}

addItems :: IO ()
addItems = return ()
