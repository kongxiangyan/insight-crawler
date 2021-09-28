{-# LANGUAGE OverloadedStrings #-}

module TencentCloud.Request.Base
  ( hXTCAction, hXTCTimestamp, hXTCVersion, hXTCRegion,
    hXCBAuthorization, hXCBSessionToken, hXCBTimeStamp,
    RequestSettings (..),
    defaultRequestSettings, getRequestSettings, postRequestSettings,
    setMethod, setRegion, setPath, setEnv
  ) where

import qualified Data.ByteString.Lazy.UTF8 as LBU
import           Network.HTTP.Types.Header

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
