{-# LANGUAGE OverloadedStrings #-}

module Notify.General
  (
    getSubscriptions,
    notifyAll,
    Message (..)
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.UTF8      as LBU
import           Data.Maybe
import           Data.Tuple.Sequence
import qualified Data.Vector                    as V
import           Network.HTTP.Client            hiding (path)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import           TencentCloud.Request.Base      hiding (method, requestBody)
import qualified TencentCloud.Request.CloudBase as R

subscriptionsCollectionsName :: String
subscriptionsCollectionsName = "Subscriptions"

{-------------------------------------------------------------------------------------------------------
                                                  获取订阅的 URL
-------------------------------------------------------------------------------------------------------}

parseSubscriptions :: LBU.ByteString -> Maybe [(String, String)]
-- parseSubscriptions raw = sequenceA . map sequenceT <$> _decodedList
parseSubscriptions raw = traverse sequenceT =<< _decodedList
  where
    _list = V.toList <$> (parseMaybe (.: "list") =<< parseMaybe (.: "data") =<< decode raw)
    _decodedList = map parseItem <$> _list
    parseItem item = (url, notifyType)
      where
        decodedItem = decode $ LBU.fromString item
        url = parseMaybe (.: "url") =<< decodedItem :: Maybe String
        notifyType = parseMaybe (.: "notifyType") =<< decodedItem :: Maybe String

getSubscriptions :: IO [(String, String)]
getSubscriptions = do
  requestSettings <- authorizedPostRequestSettings

  responseBody <- R.post requestSettings {
    path = "/databases/" ++
            subscriptionsCollectionsName ++
            "/documents:find"
  }

  print responseBody

  let parsedSubscriptions = parseSubscriptions $ LBU.fromString responseBody

  let subscriptions = fromMaybe [] parsedSubscriptions

  return subscriptions

{-------------------------------------------------------------------------------------------------------
                                                  发送通知
-------------------------------------------------------------------------------------------------------}

-- @refer: https://work.weixin.qq.com/api/doc/90000/90136/91770
workWeixinNotifyText :: Message -> String
-- workWeixinNotifyText mes = "{\"msgtype\":\"text\",\"text\":{\"content\":\"" ++ workWeixin mes ++ "\"}}"
workWeixinNotifyText mes = LBU.toString . encode $
  object ["msgtype" .= ("text" :: String), "text" .= text]
    where
      text = object ["content" .= workWeixin mes]

generalNotifyText :: Message -> String
generalNotifyText = general

buildNotifyText :: String -> Message -> String
buildNotifyText "WORK_WEIXIN" = workWeixinNotifyText
buildNotifyText "GENERAL"     = generalNotifyText
buildNotifyText _             = generalNotifyText

sendMessage :: String -> String -> IO String
sendMessage mes url = do
  manager <- newManager tlsManagerSettings
  initRequest <- parseRequest url

  let request = initRequest {
    method = "POST",
    requestHeaders = [(hContentType, "application/json")],
    requestBody = RequestBodyLBS $ LBU.fromString mes
  }

  print mes

  response <- httpLbs request manager

  putStrLn $ " - The status code was: " ++ show (statusCode $ responseStatus response)

  let body = LBU.toString $ responseBody response
  print body

  return body

data Message
  = Message
      { workWeixin :: String
      , general    :: String
      }

-- 遍历订阅地址，并依次发送指定的消息
notifyAll :: Message -> IO [String]
notifyAll mes = do
  subscriptions <- getSubscriptions
  let tasks = map (\(url, notifyType) -> sendMessage (buildNotifyText notifyType mes) url) subscriptions
  sequenceA tasks
