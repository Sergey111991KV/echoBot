<<<<<<< HEAD
{-# LANGUAGE TypeSynonymInstances #-}
module Adapter.VK.VKBot where

import ClassyPrelude
  
import Data.Aeson
    ( eitherDecode, Array, Result(Error), Value(String, Number) )
=======
module Adapter.VK.VKBot where

import ClassyPrelude
    ( ($),
      Eq((==)),
      Monad(return),
      Show(show),
      Semigroup((<>)),
      Integer,
      Either(..),
      String,
      Text,
      MonadIO(liftIO),
      (.),
      (&&),
      unpack,
      asks,
      swapTVar,
      atomically,
      readTVarIO )
    
import Network.HTTP.Client
    ( httpLbs, parseRequest, Response(responseBody) ) 
import Data.Aeson ( eitherDecode, Array, Value(String, Number) )
>>>>>>> master2
import Control.Monad.Except
    ( MonadError(throwError) )
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Has (Has(getter))
import Data.Scientific (Scientific(coefficient))
import qualified Data.Vector as V
<<<<<<< HEAD
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Conduit
    ( parseRequest, Response(responseBody), httpLbs )
import qualified  Network.HTTP.Simple as Simple
import Adapter.VK.VKConfig
    ( VKUrl(VKUrl),
      VKVersion(takeVKVersion),
      VKToken(takeVKToken),
      State(..),
      VKMonad,
      StaticState(waits, getLongPollUrl, accessToken, version),
      DynamicState(longConfig) )
=======


import Adapter.VK.VKConfig
    ( DynamicState(longConfig),
      State(..),
      StaticState(waits, getLongPollUrl, vkManager, sendMsgUrl,
                  accessToken, version),
      VKMonad,
      VKToken(takeVKToken),
      VKVersion(takeVKVersion) )
>>>>>>> master2
import Adapter.VK.VKEntity
  ( MessageVK(MessageVK)
  , ResponseVK(ResponseVK)
  , UpdatesVK(UpdatesVK)
  , VKLongPollConfig(key, server, tsLast)
  )
<<<<<<< HEAD
import Adapter.VK.VKRequest (msgSendVK, urlEncodeVars)
-- import Adapter.VK.VKRequest hiding (instance MonadHttp)  -- Why this error??
import Bot.Error
    ( Error(CannotSendMsgHelp, NotAnswer, CantConvertFromData,
            CantConvertFromArray, CannotSendMsg) )
 
import Bot.Message (BotCompatibleMessage(chatId, textMsg), BotMsg(..))
import qualified Log.ImportLog as Log
=======
import Bot.Request ( urlEncodeVars, sendRequestUrl )
import Bot.Error
    ( Error(CantConvertFromArray, NotAnswer, CantConvertFromData) )
import Bot.Message (BotCompatibleMessage(chatId, textMsg), BotMsg(..))
>>>>>>> master2


getMsgLast :: VKMonad r m => m BotMsg
getMsgLast = do
  (State dyn stat) <- getVKConfig 
  stDyn <- readTVarIO dyn
  let url =
                "https://" <>
                server (longConfig  stDyn) <>
                "?act=a_check&key=" <>
                key (longConfig  stDyn) <>
                "&ts=" <>
                show (tsLast $ longConfig  stDyn) <>
                "&wait=" <> show (waits  stat)
<<<<<<< HEAD
  manager <- liftIO newTlsManager
  request <- liftIO $ parseRequest url
  responseLastMsg <- Network.HTTP.Conduit.httpLbs request manager
=======
  request <- liftIO $ parseRequest url
  responseLastMsg <- liftIO $ httpLbs request (vkManager stat)
>>>>>>> master2
  caseOfGetMsg responseLastMsg
   

getVKConfig :: VKMonad r m => m  State
getVKConfig = do
  st <- asks getter
  let bodyReq =
        urlEncodeVars
          [ ("access_token", takeVKToken . accessToken $ staticState st)
          , ("v", show . takeVKVersion . version $ staticState st)
          ]
  let url = getLongPollUrl (staticState st) <> bodyReq
<<<<<<< HEAD
  manager <- liftIO newTlsManager
  request <- liftIO $ parseRequest url
  responseConfig <- Network.HTTP.Conduit.httpLbs request manager
  let upd =
        eitherDecode (Simple.getResponseBody responseConfig) :: Either String ResponseVK
=======
  request <- liftIO $ parseRequest url
  responseConfig <- liftIO $ httpLbs request  (vkManager $ staticState st)
  let upd =
        eitherDecode (responseBody responseConfig) :: Either String ResponseVK
>>>>>>> master2
  case upd of
    Left _ -> do
      throwError NotAnswer
    Right (ResponseVK vkconfigpoll) -> do
      getNewStateLongPool vkconfigpoll
  
getNewStateLongPool :: VKMonad r m => VKLongPollConfig -> m State
getNewStateLongPool newLongPoll = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  let newDynSt = dynSt {longConfig = newLongPoll}
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newDynSt
  asks getter
  
caseOfGetMsg ::
     VKMonad r m
  => Response Data.ByteString.Lazy.Internal.ByteString
  -> m BotMsg
caseOfGetMsg responseGetMsg = do
<<<<<<< HEAD
  print responseGetMsg
=======
>>>>>>> master2
  let upd =
        eitherDecode $ responseBody responseGetMsg :: Either String UpdatesVK
  case upd of
    Left _ -> do
      throwError CantConvertFromData
    Right (UpdatesVK ts arr) -> do
      setNewTs ts
      parseArrays arr

setNewTs :: VKMonad r m => Integer -> m  ()
setNewTs ts = do
  st <- asks getter 
  dynSt <- readTVarIO $ dynamicState st 
  let newDynSt = dynSt {longConfig =  (longConfig dynSt) {tsLast = ts} }
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newDynSt
  return ()

parseArrays :: VKMonad r m => [Array] -> m BotMsg
parseArrays [] = do
  throwError CantConvertFromArray
<<<<<<< HEAD
parseArrays (x:xs) = do
  case V.length x of
    7 -> parseArray x
    _ -> parseArrays xs
=======
parseArrays (x:xs) = 
  if V.length x == 7 &&  (parseValueInt (x V.! 0) == 4) then parseArray x else parseArrays xs
>>>>>>> master2

parseArray :: VKMonad r m => Array -> m BotMsg
parseArray arr = do
  let x = parseValueInt $ arr V.! 0
  let y = parseValueInt $ arr V.! 1
  let e = parseValueInt $ arr V.! 2
  let r = parseValueInt $ arr V.! 3
  let t = parseValueInt $ arr V.! 4
  let i = parseValueText $ arr V.! 5
  let u = parseValueText $ arr V.! 6
  return $ BotMsg (MessageVK x y e r t i u)

parseValueInt :: Value -> Integer
parseValueInt (Number a) = coefficient a
parseValueInt _ = 0

parseValueText :: Value -> Text
parseValueText (String a) = a
parseValueText _ = "not parse"

sendMsg :: VKMonad r m => BotMsg -> m ()
<<<<<<< HEAD
sendMsg (BotMsg msg) = do
  st <- asks getter 
  let url = "api.vk.com"
  respMaybe <-
    liftIO $
    msgSendVK
      (VKUrl url)
      (textMsg msg)
      (accessToken (staticState st))
      (version $ staticState st)
      (chatId msg)
  case respMaybe of
    Error _ -> do
      throwError CannotSendMsg
    resp -> do
      Log.writeLogD $ pack ("sendMsg VK " <> show resp)
      return ()
=======
sendMsg (BotMsg msg) =  do
  st <- asks getter 
  liftIO $ sendRequestUrl
    (vkManager $ staticState st)
    (sendMsgUrl $ staticState st)
       [    ("access_token", takeVKToken $ accessToken (staticState st))
          , ("v", show . takeVKVersion . version $ staticState st)
          , ("user_id", show $ chatId msg)
          , ("message" , unpack $ textMsg msg)
        ]
>>>>>>> master2

sendMsgHelp :: VKMonad r m => Text -> BotMsg -> m  ()
sendMsgHelp helpMess (BotMsg msg) = do
  st <- asks getter 
<<<<<<< HEAD
  let url = "api.vk.com"
  respMaybe <-
    liftIO $
    msgSendVK
      (VKUrl url)
      helpMess
      (accessToken (staticState st))
      (version $ staticState st)
      (chatId msg)
  case respMaybe of
    Error _ -> do
      throwError CannotSendMsgHelp
    resp -> do
      Log.writeLogD $ pack ("sendMsg VK " <> show resp)
      return  ()

getNameAdapter :: VKMonad r m => m Text
getNameAdapter = return "VK"
=======
  liftIO $ sendRequestUrl
    (vkManager $ staticState st)
    (sendMsgUrl $ staticState st)
       [    ("access_token", takeVKToken $ accessToken (staticState st))
          , ("v", show . takeVKVersion . version $ staticState st)
          , ("user_id", show $ chatId msg)
          , ("message" , unpack  helpMess)
        ]

getNameAdapter :: VKMonad r m => m Text
getNameAdapter = return "VK"


    
>>>>>>> master2
