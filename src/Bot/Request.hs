module Bot.Request
   where

import ClassyPrelude
  
import Control.Monad.Except
  

import Network.HTTP.Client
   
import Control.Arrow ( ArrowChoice(left) )
  
import Data.Aeson 
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.Char (isAlphaNum, isAscii)
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import qualified Prelude as P
import Bot.Error 


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

buildJsonObject :: [(String, String)] -> Value
buildJsonObject =
  object . fmap (\(a, b) -> (T.pack a, String (T.pack b)))

sendJSON :: (FromJSON a, ToJSON b, MonadError Error m, MonadIO m) => Manager -> String ->  b -> m a
sendJSON manager url objToDecode = do
  resp <- sendJSONraw manager url objToDecode 
  let respDec = eitherDecode $ responseBody resp
  case respDec of
    Left  _ -> throwError ErrorDecodeData
    Right objFromDecode -> return  objFromDecode


sendJSON' :: (MonadError Error m, MonadIO m, ToJSON b) => Manager -> String -> b -> m ()
sendJSON' manager url objToDecode  = void $ sendJSONraw manager url objToDecode

sendJSONraw :: (MonadError Error m, MonadIO m, ToJSON b) => Manager -> String -> b -> m (Response LBS.ByteString)
sendJSONraw manager url obj = do
  initReq <- liftIO $ parseRequest url
  let req = initReq
            { method = "POST"
            , requestBody =  RequestBodyLBS $ encode obj
            , requestHeaders = [(HTTP.hContentType, "application/json")]
            }
  resp <- liftIO  $ left httpToMyExept <$> try (httpLbs req manager)
  case resp of
    Left e -> throwError e
    Right rest -> return rest
  where
      httpToMyExept (_ :: HttpException) = HttpExceptionBot


sendReq :: (MonadError Error m, MonadIO m) => Manager -> String -> [(String, String)] -> m (Response LBS.ByteString)
sendReq manager url urlEncArray =   do
  let optionUrl = urlEncodeVars urlEncArray
  let mainUrl = url ++ "?" ++ optionUrl
  initReq <- liftIO $ parseRequest mainUrl
  let req = initReq
            { method = "POST"
            , requestHeaders = [(HTTP.hContentType, "application/x-www-form-urlencoded")] 
            }
  resp <- liftIO  $ left httpToMyExept <$> try (httpLbs req manager)
  case resp of
    Left e -> throwError e
    Right rest -> return rest
  where
      httpToMyExept (_ :: HttpException) = HttpExceptionBot


sendReq' :: (MonadError Error m, MonadIO m) => Manager -> String -> [(String, String)] -> m ()    
sendReq'  manager url urlEncArray = void $ sendReq manager url urlEncArray 