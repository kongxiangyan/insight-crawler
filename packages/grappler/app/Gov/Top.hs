{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Gov.Top
  ( grap,
    Item (..),
    emptyItem
  ) where

import qualified Data.ByteString.Lazy.UTF8 as LBU
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

type RequestResponse = Response LBU.ByteString
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
    scopedLog $ "The grap time(after response) was: " ++ generalFormatTime grapTime
    scopedLog $ "The response status was: " ++ show (responseStatus response)

    let body = LBU.toString $ responseBody response

    destination <- pathInAppDataDir "TopGov.txt"
    writeFile destination body

    scopedLog $ "Response body write to: " ++ destination
    scopedLog "Request Execute Done! \n"

    return (target, request, response, body, grapTime)) responses

-- 将 HTML 转换为可以进一步解析的格式
parseBody :: String -> [Tag String]
parseBody = parseTags

-- 提取合适的部分
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
type ItemPublishTime = Integer
type ItemGrapTime = Integer
-- RawItem 中的数据是未格式化的，数字、日期等全部都是字符串格式
newtype RawItem
  = RawItem (ItemTitle, ItemUrl, String, ItemGrapTime)
  deriving (Show)
-- Item 中的数据是格式化之后的，数字、日期等具备正确的格式，能够直接被程序的其它部分消费
newtype Item
  = Item (ItemTitle, ItemUrl, ItemPublishTime, ItemGrapTime)

instance Show Item where
  show (Item (title, url, publishTime, grapTime)) =
    "(" ++ title ++ ", " ++ url ++ ", " ++ show publishTime ++ ", " ++ show grapTime ++ ")"

emptyItem :: Item
emptyItem = Item ("", "", 0, 0)

extractData :: ItemGrapTime -> [Tag String] -> RawItem
extractData grapTime tags = RawItem (title, url, publishTime, grapTime)
  where
    url = fromAttrib "href" . head $ tags
    title = fromTagText (tags !! 1)
    publishTime = fromTagText (tags !! 2)

formatData :: RawItem -> Item
formatData (RawItem (title, url, publishTime, grapTime)) =
  Item (formatTitle title, formatUrl url, formatPublishTime publishTime, grapTime)
    where
      formatTitle = trim
      -- 有的链接没有 domain，需要进行补全
      formatUrl []       = []
      formatUrl ('/':xs) = trim $ "http://www.gov.cn/" ++ xs
      formatUrl u        = trim u
      -- 无法解析的发布日期统一赋值为 0（即 CST 1970-01-01 08:00:00）
      formatPublishTime = maybe 0 getTimestamp . parseGeneralFormatDate . trim
      trim = filter (not . isSpace)

grap :: IO [Item]
grap = do
  [(_, _, _, body, grapTime)] <- execRequest

  let formattedData =
        map (formatData . extractData (getTimestamp . zonedTimeToUTC $ grapTime)) . extractList $ parseBody body
      count = length formattedData

  destination <- pathInAppDataDir "formattedData"
  writeFile destination (show formattedData)

  scopedLog $ "Extracted formattedData write to: " ++ destination
  scopedLog $ "Total count of data: " ++ show count
  scopedLog "Grap Done!"

  return formattedData
