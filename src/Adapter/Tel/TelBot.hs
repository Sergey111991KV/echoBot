module Adapter.Tel.TelBot where

import ClassyPrelude
   
import Data.Aeson (eitherDecode)
import Data.Has (Has(getter))
import Control.Monad.Except
    
import Network.HTTP.Client


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
  
import Bot.Message (BotCompatibleMessage(chatId, idMsg, textMsg), BotMsg(..))
import Bot.Request 


getNameAdapter :: TelMonad r m => m Text
getNameAdapter = return "Telegram"


getMsgLast :: TelMonad r m => m  BotMsg
getMsgLast = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  let url = botUrl (staticState st) <> token (staticState st) <> "/" <> getUpdates (staticState st)
  responseLastMsg <- sendReq (telManager $ staticState st) url []
  let updT = eitherDecode $ responseBody responseLastMsg :: Either String TelUpdates
  (BotMsg msg) <- processUpdates (lastMsgId dynSt) updT
  let newIdMsg = idMsg msg
  if newIdMsg == lastMsgId dynSt
    then throwError NotNewMsg
    else do
      let newState = dynSt {lastMsgId = newIdMsg}
      _ <- liftIO . atomically $ swapTVar (dynamicState st) newState
      return (BotMsg msg)


processUpdates :: TelMonad r m => Integer -> Either String TelUpdates -> m BotMsg
processUpdates idStateMsg eitherUpdates = do
  case eitherUpdates of
    Left _ -> throwError NotAnswer
    Right upd -> findLastMsg idStateMsg (result upd)

findLastMsg :: TelMonad r m =>  Integer -> [TelUpdate] -> m BotMsg
findLastMsg _ [] = throwError NotNewMsg
findLastMsg lastId [x] =
  if lastId > idMsgO
    then throwError NotNewMsg
    else return $ BotMsg (updateMsg x)
  where
    idMsgO = updateId x
findLastMsg lastId (x:xs) =
  if lastId > idMsgA
    then findLastMsg lastId xs
    else findLastMsg idMsgA (xs <> [x])
  where
    idMsgA = updateId x

sendMsg :: TelMonad r m => BotMsg -> m  ()
sendMsg (BotMsg botMsg) = do
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

sendText :: TelMonad r m => Text -> Integer -> String -> m ()
sendText txtOfMsg chatIdSendMsg sendUrl = do
  st <- asks getter
  _ <- sendJSONraw' (telManager $ staticState st) sendUrl (buildBody
       [("chat_id", show chatIdSendMsg), ("text", unpack txtOfMsg)])
  return ()
  -- liftIO $ sendRequestWithJsonBody
  --   (telManager $ staticState st)
  --   sendUrl
  --   (buildBody
  --      [("chat_id", show chatIdSendMsg), ("text", unpack txtOfMsg)])
