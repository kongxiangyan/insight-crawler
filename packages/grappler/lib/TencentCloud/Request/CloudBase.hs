{-# LANGUAGE NamedFieldPuns #-}

module TencentCloud.Request.CloudBase
  ( get, post
  ) where

import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.UTF8   as LBU
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import qualified TencentCloud.Request.Base   as R
import           TencentCloud.Sign.CloudBase

tcbApiUrl :: String -> String -> String -> String
tcbApiUrl region env path =
  "https://" ++ region ++ "." ++
  "tcb-api.tencentcloudapi.com/api/v2/envs/" ++
  env ++ path

get :: R.RequestSettings -> IO String
get R.RequestSettings { R.secretId, R.secretKey, R.region, R.envId, R.path, R.method } = do
  let url = tcbApiUrl region envId path

  manager <- newManager tlsManagerSettings
  initRequest <- parseRequest url

  TCBAuthorization { timestamp, authorization } <- getAuthorization secretId secretKey

  let request = initRequest {
    method = BS.pack method,
    requestHeaders = [
      (R.hXCBAuthorization, BS.pack authorization),
      (R.hXCBTimeStamp, BS.pack timestamp)
    ]
  }

  response <- httpLbs request manager

  putStrLn $ "[TencentCloud.Request]" ++ url ++ "\n"

  putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)

  let body = LBU.toString $ responseBody response

  return body

post :: R.RequestSettings -> IO String
post R.RequestSettings { R.secretId, R.secretKey, R.region, R.envId, R.path, R.method, R.requestBody } = do
  let url = tcbApiUrl region envId path

  manager <- newManager tlsManagerSettings
  initRequest <- parseRequest url

  TCBAuthorization { timestamp, authorization } <- getAuthorization secretId secretKey

  let request = initRequest {
    method = BS.pack method,
    requestHeaders = [
      (R.hXCBAuthorization, BS.pack authorization),
      (R.hXCBTimeStamp, BS.pack timestamp),
      -- 这里必须设置 application/json
      (hContentType, BS.pack "application/json; charset=utf-8")
    ],
    requestBody = RequestBodyLBS requestBody
  }

  response <- httpLbs request manager

  putStrLn $ "[TencentCloud.Request]" ++ url ++ "\n"

  putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)

  let body = LBU.toString $ responseBody response

  return body
