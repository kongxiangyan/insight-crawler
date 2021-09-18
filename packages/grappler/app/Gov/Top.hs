{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Gov.Top
  ( grap
  ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.Char
import           Network.HTTP.Client
import           Network.HTTP.Types.Header
import           Text.HTML.TagSoup

type Name = String
type UrlString =  String
data Target
  = Target
      { name :: Name
      , url  :: UrlString
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

type RequestResponse = Response UTF8.ByteString
data RequestResult
  = RequestResult
      { target   :: Target
      , request  :: Request
      , response :: RequestResponse
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

    return (RequestResult target request response)
  )

execRequest :: IO [(Target, Request, RequestResponse, String)]
execRequest = do
  responses <- sequenceA $ prepareRequests targets
  traverse (\RequestResult { target, request, response } -> do
    putStrLn $ "The target url was:" ++ url target
    putStrLn $ "The request info was:" ++ show request
    putStrLn $ "The response status was: " ++ show (responseStatus response)

    let body = UTF8.toString $ responseBody response

    writeFile ("./packages/grappler/data/" ++ "TopGov" ++ ".txt")  body

    putStrLn "Done"

    return (target, request, response, body)) responses

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

newtype Item
  = Item (String, String, String)
extractData :: [Tag String] -> Item
extractData tags = Item (title, url, date)
  where
    url = fromAttrib "href" . head $ tags
    title = fromTagText (tags !! 1)
    date = fromTagText (tags !! 2)

instance Show Item where
  show (Item (title, url , date)) = "(" ++ title ++ ", " ++ url ++ ", " ++ date ++ ")"

formatData :: Item -> Item
formatData (Item (title, url, date)) = Item (formatTitle title, formatUrl url, formatDate date)
  where
    formatTitle = trim
    -- 有的连接没有 domain，需要进行补全
    formatUrl []       = []
    formatUrl ('/':xs) = trim $ "http://www.gov.cn/" ++ xs
    formatUrl u        = trim u
    formatDate = trim
    trim = filter (not . isSpace)

grap :: IO ()
grap = forever $
  forkIO (do
    [(target, _, _, body)] <- execRequest
    print target
    let tags = map (formatData . extractData) . extractList $ parseBody body
    print tags
    writeFile "./packages/grappler/data/tags.tmp" (show tags))
  >> threadDelay (60 * 10 * 1000000)
