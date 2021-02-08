module Adapter.Tel.TelBot where

import ClassyPrelude
  

import Control.Monad.Except (MonadError(catchError, throwError))
import Data.Has (Has(getter))
import Adapter.Tel.TelConfig
    ( DynamicState(lastMsgId),
      State(dynamicState, staticState),
      StaticState(getUpdates, delayTel, botUrl, token, textSendMsgTel,
                  telManager),
      TelMonad )
 
import Adapter.Tel.TelEntity
import Data.Aeson
import Bot.Error ( Error(CannotSendMsg) ) 
import Bot.Message
    ( BotCompatibleMsg(chatId, textMsg),
      BotMsg(..),
      findMaxUpd,
      findLastMsgs )
import Bot.Request ( buildJsonObject, sendJSON, sendJSON' )
import Log.ImportLog ( Log(writeLogD) )


getLastMsgArray :: TelMonad r m => m [BotMsg]
getLastMsgArray = do
  writeLogD "getLastMsgArray Telegram" 
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  updT <- getTelUpdates
  let arrMsg = findLastMsgs  (lastMsgId dynSt) (convertTelMes updT)
  let idMax = findMaxUpd arrMsg
  if idMax == 0
    then return arrMsg
    else do
      let newState = dynSt {lastMsgId = idMax}
      _ <- liftIO . atomically $ swapTVar (dynamicState st) newState
      return arrMsg

getTelUpdates :: TelMonad r m =>  m TelUpdates
getTelUpdates  = do
  st <- asks getter
  let url =
        botUrl (staticState st) <>
        token (staticState st) <> "/" <> getUpdates (staticState st)
  sendJSON (telManager $ staticState st) url (delayTel $ staticState st)

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


