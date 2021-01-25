module Adapter.Tel.TelBot where

import ClassyPrelude
  ( Either(..)
  , Eq((==))
  , Functor(fmap)
  , Int
  , Monad(return)
  , MonadIO(liftIO)
  , Semigroup((<>))
  , Show(show)
  , String
  , Text
  , ($)
  , (.)
  , asks
  , atomically
  , readTVarIO
  , swapTVar
  , unpack
  )

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
  ( BotCompatibleMsg(chatId, textMsg)
  , BotMsg(..)
  , findLastMsgs
  , findMaxUpd
  )
import Bot.Request (buildJsonObject, sendJSON', sendReq)
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
                                          -- [("timeout","1000")] -  я попробовал этот параметр отправить,
                                          -- но что-то не сраслось, запрос проходит, а timeoutа нет
  let updT =
        eitherDecode $ responseBody responseLastMsg :: Either String TelUpdates
  arrMsg <- processUpdates (lastMsgId dynSt) updT
  let idMax = findMaxUpd arrMsg
  if idMax == 0
    then return arrMsg
    else do
      let newState = dynSt {lastMsgId = idMax}
      _ <- liftIO . atomically $ swapTVar (dynamicState st) newState
      return arrMsg

processUpdates :: TelMonad r m => Int -> Either String TelUpdates -> m [BotMsg]
processUpdates idStateMsg eitherUpdates = do
  writeLogD "processUpdates Telegram" 
  case eitherUpdates of
    Left _ -> do
      throwError NotAnswer
    Right upd -> do
      return . findLastMsgs idStateMsg $ convertTelMes upd

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


