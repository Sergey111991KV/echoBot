module Adapter.Tel.TelBot where

import ClassyPrelude

-- import qualified Prelude as  P  
import Data.Aeson (eitherDecode)
import Data.Has (Has(getter))
import Control.Monad.Except ( MonadError(catchError, throwError) )
    
import Network.HTTP.Client ( Response(responseBody) )

import Control.Concurrent ( threadDelay )
import Adapter.Tel.TelConfig
    ( DynamicState(lastMsgId),
      State(dynamicState, staticState),
      StaticState(getUpdates, botUrl, token, textSendMsgTel, telManager),
      TelMonad )
import Adapter.Tel.TelEntity
  
import Bot.Error
  
import Bot.Message
import Bot.Request  

getLastMsgArray :: TelMonad r m => m  [BotMsg]
getLastMsgArray = do
  liftIO (threadDelay 1000000)
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  let url = botUrl (staticState st) <> token (staticState st) <> "/" <> getUpdates (staticState st)
  responseLastMsg <- sendReq (telManager $ staticState st) url []
  let updT = eitherDecode $ responseBody responseLastMsg :: Either String TelUpdates
  arrMsg <- processUpdates (lastMsgId dynSt) updT
  let idMax = findMaxUpd arrMsg
  if idMax == 0 then 
    return arrMsg
  else do
    let newState = dynSt {lastMsgId = idMax}
    _ <- liftIO . atomically $ swapTVar (dynamicState st) newState
    return arrMsg

processUpdates :: TelMonad r m => Int -> Either String TelUpdates -> m [ BotMsg]
processUpdates idStateMsg eitherUpdates = do
  case eitherUpdates of
    Left _ -> do
      throwError NotAnswer
    Right upd -> do
      return . findLastMsgs idStateMsg $ convertTelMes upd

convertTelMes :: TelUpdates -> [BotMsg]
convertTelMes telUpd = fmap fp (result telUpd)
    where
      fp a = BotMsg $ updateMsg a


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

sendText :: TelMonad r m => Text -> Int -> String -> m ()
sendText txtOfMsg chatIdSendMsg sendUrl = do
  st <- asks getter
  sendJSON' (telManager $ staticState st) sendUrl (buildJsonObject [("chat_id", show chatIdSendMsg), ("text", unpack txtOfMsg)])
  
getNameAdapter :: TelMonad r m => m Text
getNameAdapter = return "Telegram"