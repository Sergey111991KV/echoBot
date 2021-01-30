module Adapter.Tel.TelBot where

import ClassyPrelude
  

import Control.Monad.Except (MonadError(catchError, throwError))
import Data.Aeson (eitherDecode)
import Data.Has (Has(getter))

import Network.HTTP.Client (Response(responseBody))

import Adapter.Tel.TelConfig
  ( DynamicState(lastMsgId)
  , State(dynamicState, staticState)
  , StaticState(botUrl, getUpdates, telManager, textSendMsgTel, token)
  , TelMonad
  )
import Adapter.Tel.TelEntity (TelUpdate(updateMsg), TelUpdates(result))
import Control.Concurrent (threadDelay)

import Bot.Error (Error(CannotSendMsg, NotAnswer))

import Bot.Message
  
import Bot.Request
import Log.ImportLog ( Log(writeLogD) )

getLastMsgArray :: TelMonad r m => m [BotMsg]
getLastMsgArray = do
  writeLogD "getLastMsgArray Telegram" 
  liftIO (threadDelay 1000000) --- оставил ее здесь
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  let url =
        botUrl (staticState st) <>
        token (staticState st) <> "/" <> getUpdates (staticState st)
  responseLastMsg <- sendReq (telManager $ staticState st) url []
  -- responseLastMsg <- sendJSONraw (telManager $ staticState st) url timeout
  -- timeout - создать объект? на просто  строку "timeout:10" он не реагирует
  let updT =
        eitherDecode $ responseBody responseLastMsg :: Either String TelUpdates
  arrMsg <- either (\_ -> throwError NotAnswer) (return . findLastMsgs  (lastMsgId dynSt) . convertTelMes ) updT
  let idMax = findMaxUpd arrMsg
  if idMax == 0
    then return arrMsg
    else do
      let newState = dynSt {lastMsgId = idMax}
      _ <- liftIO . atomically $ swapTVar (dynamicState st) newState
      return arrMsg


convertTelMes :: TelUpdates -> [BotMsg]
convertTelMes telUpd = fmap fp (result telUpd)
  where
    fp a = BotMsg $ updateMsg a

sendMsg :: TelMonad r m => BotMsg -> m ()
sendMsg (BotMsg botMsg) = do
  writeLogD "sendMsg Telegram" 
  st <- asks getter
  let txtOfMsg = textMsg botMsg
      idM = chatId botMsg
  let url =
        botUrl (staticState st) <>
        token (staticState st) <> "/" <> textSendMsgTel (staticState st)
  sendText txtOfMsg idM url `catchError` (\_ -> do throwError CannotSendMsg)

sendMsgHelp :: TelMonad r m => Text -> BotMsg -> m ()
sendMsgHelp helpText (BotMsg botMsg) = do
  writeLogD "sendMsgHelp Telegram" 
  st <- asks getter
  let idM = chatId botMsg
  let url =
        botUrl (staticState st) <>
        token (staticState st) <> "/" <> textSendMsgTel (staticState st)
  sendText helpText idM url

sendText :: TelMonad r m => Text -> Int -> String -> m ()
sendText txtOfMsg chatIdSendMsg sendUrl = do
  writeLogD "sendText Telegram" 
  st <- asks getter
  sendJSON'
    (telManager $ staticState st)
    sendUrl
    (buildJsonObject
       [("chat_id", show chatIdSendMsg), ("text", unpack txtOfMsg)])


