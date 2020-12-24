{-# LANGUAGE TypeSynonymInstances #-}
module Adapter.VK.VKBot where

import ClassyPrelude
import Bot.Request
import Data.Aeson
    ( eitherDecode, Array, Result(Error), Value(String, Number) )
import Control.Monad.Except
    ( MonadError(throwError) )
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Has (Has(getter))
import Data.Scientific (Scientific(coefficient))
import qualified Data.Vector as V
import Network.HTTP.Client.TLS (newTlsManager)
import qualified  Network.HTTP.Simple as Simple
import Adapter.VK.VKConfig
    ( VKUrl(VKUrl),
      VKVersion(takeVKVersion),
      VKToken(takeVKToken),
      State(..),
      VKMonad,
      StaticState(waits, getLongPollUrl, accessToken, version),
      DynamicState(longConfig) )
import Adapter.VK.VKEntity
  ( MessageVK(MessageVK)
  , ResponseVK(ResponseVK)
  , UpdatesVK(UpdatesVK)
  , VKLongPollConfig(key, server, tsLast)
  )
import Network.HTTP.Conduit 
import Adapter.VK.VKRequest
import Bot.Error
    ( Error(CannotSendMsgHelp, NotAnswer, CantConvertFromData,
            CantConvertFromArray, CannotSendMsg) )
 
import Bot.Message (BotCompatibleMessage(chatId, textMsg), BotMsg(..))
import qualified Log.ImportLog as Log


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
  manager <- liftIO newTlsManager
  request <- liftIO $ parseRequest url
  responseLastMsg <- Network.HTTP.Conduit.httpLbs request manager
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
  manager <- liftIO newTlsManager
  request <- liftIO $ parseRequest url
  responseConfig <- Network.HTTP.Conduit.httpLbs request manager
  let upd =
        eitherDecode (Simple.getResponseBody responseConfig) :: Either String ResponseVK
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
  print responseGetMsg
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
parseArrays (x:xs) = do
  case V.length x of
    7 -> parseArray x
    _ -> parseArrays xs

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

sendMsgHelp :: VKMonad r m => Text -> BotMsg -> m  ()
sendMsgHelp helpMess (BotMsg msg) = do
  st <- asks getter 
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


sendMsg :: VKMonad r m => BotMsg -> m ()
sendMsg (BotMsg msg) = sendTextVK (BotMsg msg)


                                    -- REQ --



sendMsgReq :: VKMonad r m => BotMsg -> m ()
sendMsgReq (BotMsg msg) = do
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


                                    -- HTTTP Client --



sendTextVK :: VKMonad r m => BotMsg -> m ()
sendTextVK (BotMsg msg) = do
  st <- asks getter 
  let url = "https://api.vk.com/method/messages.send"
  liftIO $ sendRequest
    url
    (buildRequestBody
       [    ("access_token", show . takeVKToken $ accessToken (staticState st))
          , ("v", show . takeVKVersion . version $ staticState st)
          , ("user_id", show $ chatId msg)
          , ("message" , unpack $ textMsg msg)
        ]
      --  [   ("access_token", "1e3ca5da18082a12066e5c544f0de472e2bcc8d6c774ad2a79754ecacff5bdb7573c3dd9062a6dbc8bd05")
      --     , ("v", "5.52")
      --     , ("user_id", show $ chatId msg)
      --     , ("message" , unpack $ textMsg msg)
      --   ]
        )


                        --  Conduit --


sendTextVK' :: VKMonad r m => BotMsg -> m ()
sendTextVK' (BotMsg msg) = do
  let url' = urlEncodeVars  
        [   ("access_token", "1e3ca5da18082a12066e5c544f0de472e2bcc8d6c774ad2a79754ecacff5bdb7573c3dd9062a6dbc8bd05")
          , ("v", "5.52")
          , ("user_id", show $ chatId msg)
          , ("message" , unpack $ textMsg msg)
        ]
  let url = "https://api.vk.com/method/messages.send" ++ "?" ++ url'
  print  url
  _ <- simpleHttp url
  return ()

-- "https://api.vk.com/method/messages.send?access_token=VKToken%20%7BtakeVKToken%20%3D%20%221e3ca5da18082a12066e5c544f0de472e2bcc8d6c774ad2a79754ecacff5bdb7573c3dd9062a6dbc8bd05%22%7D&v=VKVersion%20%7BtakeVKVersion%20%3D%205.52%7D&user_id=442266618&message=textMessage"
--  Это работает - но не передает body - все в строке запросе



