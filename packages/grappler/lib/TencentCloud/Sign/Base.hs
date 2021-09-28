module TencentCloud.Sign.Base
  ( tc3HMACSHA256,
    hashSHA256,
    convertToBytes,
    Date, Service, SecretId, SecretKey
  ) where

import           Crypto.Hash
import           Crypto.MAC.HMAC
import qualified Data.ByteArray       as BA
import           Data.ByteString.UTF8

-- 可以通过这个比对算法加密结果：
-- refer: https://stackoverflow.com/questions/24840383/generating-hmacsha256-hexdigest-in-haskell
-- print $ show . hashWith SHA256 . fromString $ "some test message"
-- 3cb0603701548a84d3e7408a805e270a094000f537b96a6e83a36271a3ff192f
-- print $ show . hmacGetDigest $ (hmac (fromString "key") (fromString "some test message") :: HMAC SHA256)
-- a5a36db81683537aacf8b6283121ffdb949ece609abbfe8a5fbc91cc76031edd

-- hashSHA256
-- refer: https://cloud.tencent.com/document/product/876/34813#1.-.E6.8B.BC.E6.8E.A5.E8.A7.84.E8.8C.83.E8.AF.B7.E6.B1.82.E4.B8.B2

hashSHA256 :: String -> String
hashSHA256 targetStr = show (hashWith SHA256 . fromString $ targetStr)

-- tc3HMACSHA256
-- @refer: https://cloud.tencent.com/document/product/876/34813#3.-.E8.AE.A1.E7.AE.97.E7.AD.BE.E5.90.8D
-- 一共要加密四次，`SecretDate`, `SecretService`, `SecretSigning`, `Signature`，全部使用 HAMC SHA256 算法
-- ! 只有 SecretDate 的 key 是常规 String，其它几步的 key 都是二进制（Bytes）

convertToBytes :: String -> BA.Bytes
convertToBytes targetStr = BA.convert $ fromString targetStr :: BA.Bytes

getDigest :: HMAC a -> String
getDigest = show . hmacGetDigest

type Date = String
type Service = String
type SecretId = String
type SecretKey = String

secretDate :: SecretKey -> Date -> HMAC SHA256
secretDate secretKey date = hmac key msg :: HMAC SHA256
  where
    key = fromString ("TC3" ++ secretKey)
    msg = fromString date

secretService :: HMAC SHA256 -> Service -> HMAC SHA256
secretService _secretDate service = hmac key msg :: HMAC SHA256
  where
    key = _secretDate
    msg = fromString service

secretSigning :: HMAC SHA256 -> HMAC SHA256
secretSigning _secretService = hmac key msg :: HMAC SHA256
  where
    key = _secretService
    msg = fromString "tc3_request"

rawSignature :: HMAC SHA256 -> String -> HMAC SHA256
rawSignature _secretSigning stringToSign = hmac key msg :: HMAC SHA256
  where
    key = _secretSigning
    msg = fromString stringToSign

tc3HMACSHA256 :: Date -> Service -> SecretKey -> String -> String
tc3HMACSHA256 date service secretKey stringToSign =
  let _secretDate = secretDate secretKey date
      _secretService = secretService _secretDate service
      _secretSigning = secretSigning _secretService
      _rawSignature = rawSignature _secretSigning stringToSign
  in getDigest _rawSignature
