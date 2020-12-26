module Bot.Request
   where

import ClassyPrelude
  

import Network.HTTP.Client
  
import Data.Aeson (Value(String), encode, object)
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.Char (isAlphaNum, isAscii)
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import qualified Prelude as P

urlEncode :: String -> String
urlEncode [] = []
urlEncode (ch:t)
  | (isAscii ch && isAlphaNum ch) || ch `elem` ("-_.~" :: [Char]) =
    ch : urlEncode t
  | not (isAscii ch) = foldr escape (urlEncode t) (eightBs [] (fromEnum ch))
  | otherwise = escape (fromEnum ch) (urlEncode t)
  where
    escape b rs = '%' : showH (b `div` 16) (showH (b `mod` 16) rs)
    showH x xs
      | x <= 9 = toEnum (o_0 + x) : xs
      | otherwise = toEnum (o_A + (x - 10)) : xs
      where
        o_0 = fromEnum '0'
        o_A = fromEnum 'A'
    eightBs :: [Int] -> Int -> [Int]
    eightBs acc x
      | x <= 0xff = x : acc
      | otherwise = eightBs ((x `mod` 256) : acc) (x `div` 256)

urlEncodeVars :: [(String, String)] -> String
urlEncodeVars ((n, v):t) =
  let (same, diff) = partition ((== n) . fst) t
   in urlEncode n ++
      '=' :
      P.foldl (\x y -> x ++ ',' : urlEncode y) (urlEncode v) (map snd same) ++
      urlEncodeRest diff
  where
    urlEncodeRest [] = []
    urlEncodeRest diff = '&' : urlEncodeVars diff
urlEncodeVars [] = []

buildBody :: [(String, String)] -> LBS.ByteString
buildBody =
  encode . object . fmap (\(a, b) -> (T.pack a, String (T.pack b)))


buildRequestWithBody :: String -> RequestBody -> IO Request
buildRequestWithBody url body = do
  nakedRequest <- parseRequest url
  return
    (nakedRequest
       { method = "POST"
       , requestBody = body
       , requestHeaders = [(HTTP.hContentType, "application/json")]
       })


sendRequestWithBody :: Manager -> String -> LBS.ByteString -> IO ()
sendRequestWithBody manager url body = do
  request <- buildRequestWithBody url (RequestBodyLBS body)
  _ <- httpLbs request manager
  return ()

sendRequestWithBody' ::  Manager ->  String -> LBS.ByteString -> IO (Response LBS.ByteString)
sendRequestWithBody' manager url body = do
  request <- buildRequestWithBody url (RequestBodyLBS body)
  httpLbs request manager




sendRequestUrl ::  Manager -> String -> [(String, String)]  -> IO ()
sendRequestUrl manager url urlEncArray = do
  let optionUrl = urlEncodeVars urlEncArray
  let mainUrl = url ++ "?" ++ optionUrl
  print mainUrl
  initReq <- parseRequest mainUrl
  let req = initReq
            { method = "POST"
            , requestHeaders = [(HTTP.hContentType, "application/x-www-form-urlencoded")] -- Is it need? Or need to create some another parametrs?
                                                                                          -- or another method?
                                                                                          -- addToRequestQueryString :: Query -> Request -> Request
                                                                                          -- setRequestBodyURLEncoded :: [(ByteString, ByteString)] -> Request -> Request
            }
  _ <- httpLbs req manager
  return ()

