{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Gov.Top
  ( grap,
    Item (Item)
  ) where

import qualified Data.ByteString.Lazy.UTF8 as BU
import           Data.Char
import           Data.Time
import           Network.HTTP.Client
import           Network.HTTP.Types.Header
import           Text.HTML.TagSoup
import           Utils.Logger
import           Utils.Path
import           Utils.Time

scopedLog :: String -> IO ()
scopedLog = stdLog "Top Grappler"

data Target
  = Target
      { name :: String
      , url  :: String
      }
  deriving (Show)
type Targets = [Target]

targets :: Targets
targets = [Target "Zhengce" "http://www.gov.cn/zhengce/zuixin.htm"]

-- 添加通用的 Proxy 配置
setCommonProxy :: ManagerSettings -> ManagerSettings
setCommonProxy =  managerSetProxy  (useProxy Proxy {
    proxyHost = "127.0.0.1",
    proxyPort = 10809
  })

-- 添加通用的 Manager 设置
setCommonManager :: ManagerSettings -> ManagerSettings
setCommonManager settings = settings { managerResponseTimeout = responseTimeoutMicro (30 * 1000000) }

type RequestResponse = Response BU.ByteString
data RequestResult
  = RequestResult
      { target   :: Target
      , request  :: Request
      , response :: RequestResponse
      , grapTime :: ZonedTime
      }
  deriving (Show)

prepareRequests :: [Target] -> [IO RequestResult]
prepareRequests = map (\target -> do

    manager <- newManager $ setCommonManager defaultManagerSettings

    initRequest <- parseRequest $ url target
    let request = initRequest {
      method = "GET",
      requestHeaders = [
        -- 不设置 User-Agent 也可以拿到数据
        (hUserAgent, "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/93.0.4577.63 Safari/537.36 Edg/93.0.961.38")
      ]
    }

    response <- httpLbs request manager

    RequestResult target request response <$> getZonedTime
  )

execRequest :: IO [(Target, Request, RequestResponse, String, ZonedTime)]
execRequest = do
  responses <- sequenceA $ prepareRequests targets
  traverse (\RequestResult { target, request, response, grapTime } -> do

    scopedLog $ "The grap target was: " ++ show target
    scopedLog $ "The grap time was: " ++ generalFormatTime grapTime
    scopedLog $ "The response status was: " ++ show (responseStatus response)

    let body = BU.toString $ responseBody response

    destination <- pathInAppDataDir "TopGov.txt"
    writeFile destination body

    scopedLog $ "Response body write to: " ++ destination
    scopedLog "Request Execute Done!"

    return (target, request, response, body, grapTime)) responses

parseBody :: String -> [Tag String]
parseBody = parseTags

extractList :: [Tag String] -> [[Tag String]]
extractList =
  map (
    concat .
    (\tags -> [
      (take 2 . dropWhile (~/= ("<a>" :: String))) tags,
      (take 1 . drop 1 . dropWhile (~/= TagOpen "span" [("class" :: String,"date")])) tags
    ]) .
    takeWhile (~/= ("</li>" :: String))
  ) .
  sections (~== ("<h4>" :: String)) .
  takeWhile (~/= TagOpen "span" [("class" :: String, "public_more")]) .
  dropWhile (~/= TagOpen "div" [("class" :: String, "news_box")] )

type ItemTitle = String
type ItemUrl = String
-- TODO: 把时间都改成 Int 格式
type ItemPublishTime = String
type ItemGrapTime = String
newtype Item
  = Item (ItemTitle, ItemUrl, ItemPublishTime, ItemGrapTime)

extractData :: ItemGrapTime -> [Tag String] -> Item
extractData grapTime tags = Item (title, url, publishTime, grapTime)
  where
    url = fromAttrib "href" . head $ tags
    title = fromTagText (tags !! 1)
    publishTime = fromTagText (tags !! 2)

instance Show Item where
  show (Item (title, url, publishTime, grapTime)) =
    "(" ++ title ++ ", " ++ url ++ ", " ++ publishTime ++ ", " ++ grapTime ++ ")"

formatData :: Item -> Item
formatData (Item (title, url, publishTime, grapTime)) =
  Item (formatTitle title, formatUrl url, formatPublishTime publishTime, grapTime)
    where
      formatTitle = trim
      -- 有的链接没有 domain，需要进行补全
      formatUrl []       = []
      formatUrl ('/':xs) = trim $ "http://www.gov.cn/" ++ xs
      formatUrl u        = trim u
      formatPublishTime = trim
      trim = filter (not . isSpace)

grap :: IO [Item]
grap = do
  [(_, _, _, body, grapTime)] <- execRequest

  let formattedData = map (formatData . extractData (generalFormatTime grapTime)) . extractList $ parseBody body
      count = length formattedData

  destination <- pathInAppDataDir "formattedData"
  writeFile destination (show formattedData)

  scopedLog $ "Extracted formattedData write to: " ++ destination
  scopedLog $ "Total count of data: " ++ show count
  scopedLog "Grap Done!"

  return formattedData
