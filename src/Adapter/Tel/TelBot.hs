module Adapter.Tel.TelBot where

import ClassyPrelude
    ( ($),
      Eq((==)),
      Monad(return),
      Ord((>)),
      Show(show),
      Semigroup((<>)),
      Integer,
      Either(..),
      String,
      Text,
      MonadIO(liftIO),
      (.),
      print,
      unpack,
      asks,
      swapTVar,
      atomically,
      readTVarIO )
  
import Control.Exception (catch)
import Data.Aeson (eitherDecode)
import Data.Has (Has(getter))
import Network.HTTP.Conduit (HttpException, simpleHttp)
import Control.Monad.Except
    ( MonadError(catchError, throwError) )
    
import Adapter.Tel.TelConfig
    ( State(dynamicState, staticState),
      TelMonad,
      StaticState(getUpdates, botUrl, token, textSendMsgTel),
      DynamicState(lastMsgId) )
 

import Adapter.Tel.TelEntity
  ( TelUpdate(updateId, updateMsg)
  , TelUpdates(result)
  )
import Bot.Error
    ( Error(CannotSendMsgHelp, NotAnswer, NotNewMsg, CannotSendMsg) ) 
import Bot.Message (BotCompatibleMessage(chatId, idMsg, textMsg), BotMsg(..))
import Bot.Request (buildRequestBody, sendRequest)

getNameAdapter :: TelMonad r m => m Text
getNameAdapter = return "Telegram"

getMsgLast :: TelMonad r m => m  BotMsg
getMsgLast = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  let url = botUrl (staticState st) <> token (staticState st) <> "/" <> getUpdates (staticState st)
  upd <-
    liftIO . Control.Exception.catch (simpleHttp url) $ \e -> do
      print (e :: HttpException)
      return "HttpException"
  let updT = eitherDecode upd :: Either String TelUpdates
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
      -- print (e :: HttpException) -- this maybe be helpful
      throwError CannotSendMsg )

sendMsgHelp :: TelMonad r m => Text -> BotMsg -> m  ()
sendMsgHelp helpText (BotMsg botMsg) = do
  st <- asks getter
  let idM = chatId botMsg
  let url = botUrl (staticState st) <> token (staticState st) <> "/" <> textSendMsgTel (staticState st)
  sendText helpText idM url `catchError`  (\_ -> do
      throwError CannotSendMsgHelp )
  

sendText :: TelMonad r m => Text -> Integer -> String -> m ()
sendText txtOfMsg chatIdSendMsg sendUrl =
  liftIO $ sendRequest
    sendUrl
    (buildRequestBody
       [("chat_id", show chatIdSendMsg), ("text", unpack txtOfMsg)])
