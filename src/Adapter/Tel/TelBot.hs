module Adapter.Tel.TelBot where

import ClassyPrelude
    
import Control.Concurrent ( threadDelay )  
import Data.Aeson (eitherDecode)
import Data.Has (Has(getter))
import Control.Monad.Except ( MonadError(catchError, throwError) )
    
import Network.HTTP.Client ( Response(responseBody) )


import Adapter.Tel.TelConfig
    ( DynamicState(lastMsgId),
      State(dynamicState, staticState),
      StaticState(getUpdates, botUrl, token, textSendMsgTel, telManager),
      TelMonad )
import Adapter.Tel.TelEntity
  ( TelUpdate(updateId, updateMsg)
  , TelUpdates(result)
  )
import Bot.Error
  
import Bot.Message
import Bot.Request  


getNameAdapter :: TelMonad r m => m Text
getNameAdapter = return "Telegram"


getLastMsgArray :: TelMonad r m => m  [BotMsg]
getLastMsgArray = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  let url = botUrl (staticState st) <> token (staticState st) <> "/" <> getUpdates (staticState st)
  responseLastMsg <- sendReq (telManager $ staticState st) url []
  let updT = eitherDecode $ responseBody responseLastMsg :: Either String TelUpdates
  arrMsg <- processUpdates (lastMsgId dynSt) updT
  newIdMsg <-  findUpdTel arrMsg
  let newState = dynSt {lastMsgId = newIdMsg}
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newState
  return arrMsg

findUpdTel :: TelMonad r m => [BotMsg] -> m Int
findUpdTel = undefined

processUpdates :: TelMonad r m => Int -> Either String TelUpdates -> m [BotMsg]
processUpdates idStateMsg eitherUpdates = do
  case eitherUpdates of
    Left _ -> throwError NotAnswer
    Right upd -> do
      findLastMsgs idStateMsg (result upd)
      

findLastMsgs :: TelMonad r m =>  Int -> [TelUpdate] -> m [BotMsg]
findLastMsgs _ [] = return []
findLastMsgs lastId arr = do
  return $ mapMaybe (findMsg lastId)  arr

findMsg :: Int -> TelUpdate ->   Maybe BotMsg
findMsg lastId telUpd = if lastId > idMsgO
                            then  Just $ BotMsg (updateMsg telUpd) else Nothing
                        where
                           idMsgO = updateId telUpd


sendMsg :: TelMonad r m => BotMsg -> m  ()
sendMsg (BotMsg botMsg) = do
  liftIO (threadDelay 1000000)
  st <- asks getter
  let txtOfMsg = textMsg botMsg
      idM = chatId botMsg
  let url = botUrl (staticState st) <> token (staticState st) <> "/" <> textSendMsgTel (staticState st)
  sendText txtOfMsg idM url `catchError`  (\_ -> do
      throwError CannotSendMsg )

sendMsgHelp :: TelMonad r m => Text -> BotMsg -> m  ()
sendMsgHelp helpText (BotMsg botMsg) = do
  st <- asks getter
  let idM = chatId botMsg
  let url = botUrl (staticState st) <> token (staticState st) <> "/" <> textSendMsgTel (staticState st)
  sendText helpText idM url 

sendText :: TelMonad r m => Text -> Int -> String -> m ()
sendText txtOfMsg chatIdSendMsg sendUrl = do
  st <- asks getter
  sendJSON' (telManager $ staticState st) sendUrl (buildJsonObject [("chat_id", show chatIdSendMsg), ("text", unpack txtOfMsg)])
  