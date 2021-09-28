module TencentCloud.Sign.CloudBase
  ( getAuthorization,
    TCBAuthorization (..)
  ) where

import           Data.Time
import           TencentCloud.Sign.Base
import           Utils.Time

{-

- 腾讯的云 API 有一致的签名规范，叫做：[TC3-HMAC-SHA256](https://cloud.tencent.com/document/product/876/34813)
- 云开发 Open API 的签名规范基本遵循了 TC3-HMAC-SHA256，但存在一些区别
    - [Open API 参考 - 云开发 CloudBase](https://docs.cloudbase.net/api-reference/openapi/introduction)
    - 具体的区别见下文代码中的注释

-}

tcbHost :: String
tcbHost = "api.tcloudbase.com"

tcbContentType :: String
tcbContentType = "application/json; charset=utf-8"

tcbSignatureVersion :: String
tcbSignatureVersion = "1.0"

{-------------------------------------------------------------------------------------------------------
                                                  初始数据
-------------------------------------------------------------------------------------------------------}

currentTime :: IO UTCTime
currentTime = getCurrentTime

{-------------------------------------------------------------------------------------------------------
                                                  拼接规范请求串
-------------------------------------------------------------------------------------------------------}

-- @refer: https://cloud.tencent.com/document/product/876/34813#1.-.E6.8B.BC.E6.8E.A5.E8.A7.84.E8.8C.83.E8.AF.B7.E6.B1.82.E4.B8.B2
-- @refer: https://docs.cloudbase.net/api-reference/openapi/introduction#1-canonicalrequest

-- 云开发 Open API 的签名中 httpRequestMethod 参数固定为 "POST"
-- httpRequestMethod = "GET"
httpRequestMethod :: String
httpRequestMethod = "POST"
-- 云开发 Open API 的签名中 canonicalURI 参数固定为 "//api.tcloudbase.com/"
-- canonicalURI = "/"
canonicalURI :: String
canonicalURI = "//api.tcloudbase.com/"
-- 云开发 Open API 的签名中 canonicalQueryString 参数固定为 ""
canonicalQueryString :: String
canonicalQueryString = ""
-- 云开发 Open API 的签名中 canonicalHeaders 参数固定为 "content-type:application/json; charset=utf-8\nhost:api.tcloudbase.com\n"
-- canonicalHeaders = "content-type:application/x-www-form-urlencoded; charset=utf-8\nhost:tcb-api.tencentcloudapi.com\n"
canonicalHeaders :: String
canonicalHeaders = "content-type:" ++ tcbContentType ++ "\nhost:" ++ tcbHost ++ "\n"
-- 云开发 Open API 的签名中 signedHeaders 参数固定为 "content-type;host"
signedHeaders :: String
signedHeaders = "content-type;host"
-- 云开发 Open API 的签名中 requestPayload 参数固定为 ""
-- requestPayload = "{}"
requestPayload :: String
requestPayload = ""
hashedRequestPayload :: String
hashedRequestPayload = hashSHA256 requestPayload

generateCanonicalRequest :: IO String
generateCanonicalRequest = return canonicalRequest
  where
    canonicalRequest =
      httpRequestMethod ++ "\n" ++
      canonicalURI ++ "\n" ++
      canonicalQueryString ++ "\n" ++
      canonicalHeaders ++ "\n" ++
      signedHeaders ++ "\n" ++
      hashedRequestPayload

{------------------------------------------------------------------------------------------------------
                                                  拼接待签名字符串
------------------------------------------------------------------------------------------------------}

-- @refer: https://cloud.tencent.com/document/product/876/34813#2.-.E6.8B.BC.E6.8E.A5.E5.BE.85.E7.AD.BE.E5.90.8D.E5.AD.97.E7.AC.A6.E4.B8.B2

algorithm :: String
algorithm = "TC3-HMAC-SHA256"
serviceName :: Service
serviceName = "tcb"
requestTimestamp :: String -> String
requestTimestamp stamp = stamp
credentialScope :: String -> String
credentialScope date = date ++ "/" ++ serviceName ++ "/tc3_request"
hashedCanonicalRequest :: String -> String
hashedCanonicalRequest _canonicalRequest = hashSHA256 _canonicalRequest

generateStringToSign :: String -> Date -> String -> IO String
generateStringToSign stamp date canonicalRequest = return stringToSign
  where
    stringToSign =
      algorithm ++ "\n" ++
      requestTimestamp stamp ++ "\n" ++
      credentialScope date ++ "\n" ++
      hashedCanonicalRequest canonicalRequest

{-------------------------------------------------------------------------------------------------------
                                                  计算签名
-------------------------------------------------------------------------------------------------------}

-- @refer: https://cloud.tencent.com/document/product/876/34813#3.-.E8.AE.A1.E7.AE.97.E7.AD.BE.E5.90.8D

generateSignature :: String -> String -> SecretKey -> IO String
generateSignature date secretKey stringToSign = return $ tc3HMACSHA256 date serviceName secretKey stringToSign

{-------------------------------------------------------------------------------------------------------
                                                  拼接 Authorization
-------------------------------------------------------------------------------------------------------}

-- 构造 Cloudbase Open API 的 Authorization

-- @refer: https://cloud.tencent.com/document/product/876/34813#4.-.E6.8B.BC.E6.8E.A5-Authorization

generateAuthorization :: SecretId -> SecretKey -> IO TCBAuthorization
generateAuthorization secretId secretKey = do
  time <- currentTime
  let stamp = show $ getTimestamp time
      date = generalFormatDate time

  -- putStrLn $ stamp ++ "/" ++ date

  canonicalRequest <- generateCanonicalRequest
  stringToSign <- generateStringToSign stamp date canonicalRequest
  _signature <- generateSignature date secretKey stringToSign

  let _authorization =
        -- 云开发 Open API 的认证字符串需要加认证签名版本号
        -- 看过云开发的签名源码之后，这是一个非常合理的决定，为之后优化升级预留操作空间
        -- @refer: https://docs.cloudbase.net/api-reference/openapi/introduction#5-%E5%8A%A0%E4%B8%8A-cloudbase-%E7%AD%BE%E5%90%8D%E7%89%88%E6%9C%AC
        tcbSignatureVersion ++ " " ++
        algorithm ++ " " ++
        "Credential=" ++ secretId ++ "/" ++ credentialScope date ++ ", " ++
        "SignedHeaders=" ++ signedHeaders ++ ", " ++
        "Signature=" ++ _signature

  -- print $ "[canonicalRequest] " ++ canonicalRequest
  -- print $ "[stringToSign] " ++ stringToSign
  -- print $ "[signature] " ++ _signature
  return (TCBAuthorization stamp _authorization tcbSignatureVersion tcbContentType tcbHost)

data TCBAuthorization
  = TCBAuthorization
      { timestamp     :: String
      , authorization :: String
      , version       :: String
      , contentType   :: String
      , host          :: String
      }

getAuthorization :: SecretId -> SecretKey -> IO TCBAuthorization
getAuthorization = generateAuthorization
