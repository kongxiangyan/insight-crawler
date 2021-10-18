{-# LANGUAGE OverloadedStrings #-}

module TencentCloud.Request.Base
  ( hXTCAction, hXTCTimestamp, hXTCVersion, hXTCRegion,
    hXCBAuthorization, hXCBSessionToken, hXCBTimeStamp,
    RequestSettings (..),
    defaultRequestSettings, getRequestSettings, postRequestSettings,
    setMethod, setRegion, setPath, setEnv,
    authorizeRequestSettings, authorizedGetRequestSettings, authorizedPostRequestSettings
  ) where

import qualified Data.ByteString.Lazy.UTF8 as LBU
import           Network.HTTP.Types.Header
import qualified System.Environment        as Env

hXCBAuthorization, hXCBSessionToken, hXCBTimeStamp :: HeaderName
hXCBAuthorization = "X-CloudBase-Authorization"
hXCBSessionToken = "X-CloudBase-SessionToken"
hXCBTimeStamp = "X-CloudBase-TimeStamp"

hXTCAction, hXTCTimestamp, hXTCVersion, hXTCRegion :: HeaderName
hXTCAction = "X-TC-Action"
hXTCTimestamp = "X-TC-Timestamp"
hXTCVersion = "X-TC-Version"
hXTCRegion = "X-TC-Region"

data RequestSettings
  = RequestSettings
      { secretId    :: String
      , secretKey   :: String
      , region      :: String
      , envId       :: String
      , path        :: String
      , method      :: String
      , requestBody :: LBU.ByteString
      }

defaultRequestSettings :: RequestSettings
defaultRequestSettings = RequestSettings {
    secretId = "",
    secretKey = "",
    region = "ap-guangzhou",
    envId = "",
    path = "/",
    method = "POST",
    requestBody = ""
  }

getRequestSettings :: RequestSettings
getRequestSettings = defaultRequestSettings { method = "GET" }

postRequestSettings :: RequestSettings
postRequestSettings = defaultRequestSettings { method = "POST" }

setRegion :: RequestSettings -> String -> RequestSettings
setRegion s r = s { region = r }

setEnv :: RequestSettings -> String -> RequestSettings
setEnv s e = s { envId = e }

setPath :: RequestSettings -> String -> RequestSettings
setPath s p = s { path = p }

setMethod :: RequestSettings -> String -> RequestSettings
setMethod s m = s { method = m }

authorizeRequestSettings :: RequestSettings -> IO RequestSettings
authorizeRequestSettings baseSettings = do
  -- 从环境变量中获取云服务资源操作密钥
  _secretId <- Env.getEnv "TC_SECRET_ID"
  _secretKey <- Env.getEnv "TC_SECRET_KEY"
  _envId <- Env.getEnv "TCB_ENV_ID"

  return baseSettings {
    secretId = _secretId,
      secretKey = _secretKey,
    envId = _envId
  }

authorizedGetRequestSettings :: IO RequestSettings
authorizedGetRequestSettings = authorizeRequestSettings getRequestSettings
authorizedPostRequestSettings :: IO RequestSettings
authorizedPostRequestSettings = authorizeRequestSettings postRequestSettings
