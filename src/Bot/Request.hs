module Bot.Request
   where

import ClassyPrelude
  
import Control.Monad.Except
  

import Network.HTTP.Client
   
import Control.Arrow ( ArrowChoice(left) )
  
import Data.Aeson (Value(String), encode, object)
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


buildRequestWithBody :: String -> RequestBody -> IO Request
buildRequestWithBody url body = do
  nakedRequest <- parseRequest url
  return
    (nakedRequest
       { method = "POST"
       , requestBody = body
       , requestHeaders = [(HTTP.hContentType, "application/json")]
       })


sendRequestWithJsonBody :: Manager -> String -> LBS.ByteString -> IO ()
sendRequestWithJsonBody manager url body = void $ sendRequestWithJsonBody'  manager url body

sendRequestWithJsonBody' ::  Manager ->  String -> LBS.ByteString -> IO (Response LBS.ByteString)
sendRequestWithJsonBody' manager url body = do
  request <- buildRequestWithBody url (RequestBodyLBS body)
  httpLbs request manager


sendRequestUrl ::  Manager -> String -> [(String, String)]  -> IO ()
sendRequestUrl manager url urlEncArray = do
  let optionUrl = urlEncodeVars urlEncArray
  let mainUrl = url ++ "?" ++ optionUrl
  initReq <- parseRequest mainUrl
  let req = initReq
            { method = "POST"
            , requestHeaders = [(HTTP.hContentType, "application/x-www-form-urlencoded")] 
            }
  _ <- httpLbs req manager
  return ()

-- sendJSON :: (FromJSON a, ToJSON b, MonadError Eror) => Manager ->  b -> m a
-- sendJSON manager j = sendReq (buildReq j) >>= liftEither . eitherDecode . responseBody
--   where buildReq j =  somehtingHere
--        { method = "POST"
--        , requestBody = encode j
--        , requestHeaders = [(HTTP.hContentType, "application/json")]
--        })

sendReq :: (MonadError Error m, MonadIO m) => Manager -> String -> [(String, String)] -> m (Response LBS.ByteString)
sendReq manager url urlEncArray =   do
  let optionUrl = urlEncodeVars urlEncArray
  let mainUrl = url ++ "?" ++ optionUrl
  initReq <- liftIO $ parseRequest mainUrl
  let req = initReq
            { method = "POST"
            , requestHeaders = [(HTTP.hContentType, "application/x-www-form-urlencoded")] 
            }
  t <- liftIO  $ left httpToMyExept <$> try (httpLbs req manager)
  case t of
    Left e -> throwError e
    Right rest -> return rest
  where
      httpToMyExept (_ :: HttpException) = HttpExceptionBot


sendReq' :: (MonadError Error m, MonadIO m) => Manager -> String -> [(String, String)] -> m ()    
sendReq'  manager url urlEncArray = void $ sendReq manager url urlEncArray 

-- sendReqTest' :: (MonadError Error m, MonadIO m) => Manager -> String -> [(String, String)] -> m (Response LBS.ByteString)
-- sendReqTest' manager url  =   do
--   let optionUrl = urlEncodeVars urlEncArray
--   let mainUrl = url ++ "?" ++ optionUrl
--   initReq <- liftIO $ parseRequest mainUrl
--   let req = initReq
--             { method = "POST"
--             , requestHeaders = [(HTTP.hContentType, "application/x-www-form-urlencoded")] 
--             }
--   t <- liftIO  $ left httpToMy <$> try (httpLbs req manager)
--   case t of
--     Left _ -> throwError HttpExceptionBot
--     Right rest -> return rest
--   where
--       httpToMy (_ :: HttpException) = HttpExceptionBot


-- sendReq :: (MonadError Error m, MonadIO m) => Manager -> String -> [(String, String)] -> m (Either Error (Response LBS.ByteString))
-- sendReq manager url urlEncArray =   do
--   let optionUrl = urlEncodeVars urlEncArray
--   let mainUrl = url ++ "?" ++ optionUrl
--   initReq <- liftIO $ parseRequest mainUrl
--   let req = initReq
--             { method = "POST"
--             , requestHeaders = [(HTTP.hContentType, "application/x-www-form-urlencoded")] 
--             }
--   liftIO  $ left httpToMy <$> try (httpLbs req manager) 
--   where
--       httpToMy (_ :: HttpException) = HttpExceptionBot
  
-- sendReq' :: (MonadError Error m, MonadIO m) => Manager -> String -> [(String, String)] -> m ()
-- sendReq' manager url urlEncArray =   void $ sendReq  manager url urlEncArray
  -- do
  -- let optionUrl = urlEncodeVars urlEncArray
  -- let mainUrl = url ++ "?" ++ optionUrl
  -- initReq <- liftIO $ parseRequest mainUrl
  -- let req = initReq
  --           { method = "POST"
  --           , requestHeaders = [(HTTP.hContentType, "application/x-www-form-urlencoded")] 
  --           }
  -- liftIO  $ left httpToMy <$> try (httpLbs req manager) 
  -- where
  --     httpToMy (_ :: HttpException) = HttpExceptionBot

-- sendReq :: (MonadError Error m, MonadIO m) =>  Manager -> String -> [(String, String)]  -> m (Response LBS.ByteString)
-- --  => Manager ->  Request -> m (Either Error (Response LBS.ByteString))
-- sendReq manager url urlEncArray = do
--   -- let optionUrl = 
--   initReq <- liftIO $ parseRequest (url ++ "?" ++ urlEncodeVars urlEncArray)
--   let req = initReq
--             { method = "POST"
--             , requestHeaders = [(HTTP.hContentType, "application/x-www-form-urlencoded")] 
--             }
--   -- result <- try $ liftIO $  httpLbs req manager 

  -- case result of
  --   Left _ -> throwError NotAnswer
  --   Right a -> return a

 

