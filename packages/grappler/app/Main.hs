#!/usr/bin/env stack
{-stack
  script
  --resolver lts-18.6
  --package time
  --package memory
  --package utf8-string,bytestring,directory,filepath
  --package http-client,http-client-tls,http-conduit,http-types
  --package tagsoup
  --package cryptonite
  --package aeson unordered-containers vector
  --package tuple
  --package warp wai
-}

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8     as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBU
import           Data.List
import           GHC.IO.Encoding
import qualified Gov.Top                   as Top
import           Notify.General
import qualified Storage.TCBDB             as DB
import           Utils.Time

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp

runProcess :: IO ()
runProcess = do
  -- 抓取目标内容
  grappledItems <- Top.grap
  -- 获取数据库中的最新条目（如果数据库条目数量为零，则使用 0）
  latestItem <- DB.getLatestItem

  -- 从抓取的内容中筛选新的条目
  let Top.Item (_, _, latestPublishTime, _) = latestItem
  let newerItems = filter (\(Top.Item (_, _, t, _)) -> t > latestPublishTime) grappledItems

  print $ "latestItem is: " ++ show latestItem
  print $ "newerItems / totalItems: " ++ show (length newerItems) ++ "/" ++ show (length grappledItems)

  -- 按照发布时间升序排列，如果要降序的话，使用 Data.Ord.Down 即可
  let sortedItems = sortOn (\(Top.Item (_, _, t, _)) -> t) newerItems

  -- 将新的条目按照从早到晚的顺序插入到数据库中
  unless (null sortedItems) $ DB.addItems sortedItems

  -- 将新的条目广播给其它处理器（Web Hook）
  let workWeixinMes = makeMes sortedItems
        where
          makeMes [Top.Item (title, url, publishTime, _)] =
            "[" ++ (generalFormatDate . timestampToUTC $ publishTime) ++ "] " ++ title ++ ": " ++ url
          makeMes items@(_ : _)                  = DB.itemsToJSON items
          makeMes _                              = ""

  _ <- if  not . null $ sortedItems
        then notifyAll (Message { workWeixin = workWeixinMes, general = DB.itemsToJSON sortedItems})
        else return [""]

  return ()

listenPort :: IO ()
listenPort = run 3000 application
  where
    application _ respond = respond $
      responseLBS status200 [(hContentType, LBS.pack "text/plain")] (LBU.fromString "Hello World")

main :: IO ()
main = do
  setLocaleEncoding utf8
  _ <- forkIO listenPort
  forever $
    forkIO runProcess
    >> threadDelay (30 * 10 * 1000000)
