{-# LANGUAGE OverloadedStrings #-}

module Gov.Taiyuan where

import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.Foldable
import           Network.HTTP.Client
import           Network.HTTP.Types.Header

type UrlString = String
type TargetUrls = [UrlString]

targetUrls :: TargetUrls
targetUrls = [
    "http://taiyuan.gov.cn/intertidwebapp/govChanInfo/getDocuments?Index=1&pageSize=20&siteId=1&ChannelType=1&KeyWord=&KeyWordType=&chanId=25&order=1"
  ]

-- 添加通用的 Proxy 配置
setCommonProxy :: ManagerSettings -> ManagerSettings
setCommonProxy  = managerSetProxy (useProxy Proxy {
    proxyHost = "127.0.0.1",
    proxyPort = 10809
  })

-- 添加通用的 Manager 设置
setCommonManager :: ManagerSettings -> ManagerSettings
setCommonManager settings = settings { managerResponseTimeout = responseTimeoutMicro 30000000 }

prepareRequests = map (\url -> do
    manager <- newManager $ setCommonManager defaultManagerSettings
    initRequest <- parseRequest url
    let request = initRequest {
      method = "GET",
      requestHeaders = [
        -- 不设置 User-Agent 也可以拿到数据
        (hUserAgent, "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/93.0.4577.63 Safari/537.36 Edg/93.0.961.38")
      ]
    }
    response <- httpLbs request manager
    return (url, request, response)
  )
