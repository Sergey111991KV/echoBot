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


getNameAdapter :: TelMonad r m => m Text
getNameAdapter = return "Telegram"


getLastMsgArray :: TelMonad r m => m  [BotMsg]
getLastMsgArray = do
  liftIO (threadDelay 1000000)
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  print  "dynSt"
  print $ lastMsgId dynSt
  let url = botUrl (staticState st) <> token (staticState st) <> "/" <> getUpdates (staticState st)
  responseLastMsg <- sendReq (telManager $ staticState st) url []
  let updT = eitherDecode $ responseBody responseLastMsg :: Either String TelUpdates
  print updT
  arrMsg <- processUpdates (lastMsgId dynSt) updT
  arrMsg' <- processUpdates' (lastMsgId dynSt) updT
  let newIdMsg = findUpdTel arrMsg
  print "new idmess"
  print $ length arrMsg'
  print "getIdsMsg"
  let getIdsMsg'' = getIdsMsg arrMsg'
  print getIdsMsg''
  print "maximum'"
  let ff = maximum' (getIdsMsg arrMsg')
  if ff == 0 then 
    return arrMsg
  else do
    print ff
    let newState = dynSt {lastMsgId = ff}
    _ <- liftIO . atomically $ swapTVar (dynamicState st) newState
    return arrMsg

findUpdTel ::  [BotMsg] -> Int
findUpdTel arr = maximum' arr'
  where
     arr' = getIdsMsg arr

maximum' :: [Int] -> Int
maximum' [] = 0
maximum' [x] = x
maximum' (x:xs) = if x > head' xs then maximum' (x: tail' xs) else maximum' xs
  
    
tail' ::  [Int] -> [Int]
tail' [] = []
tail' [x] = [x]
tail' (_:xs) = xs

head' ::[Int] -> Int
head' [] = 0
head' [x] = x
head' (x:_) = x



getIdsMsg :: [BotMsg] ->  [Int]
getIdsMsg [] =  []
getIdsMsg (BotMsg x:xs) = [idMsg x] <> getIdsMsg xs

processUpdates' :: TelMonad r m => Int -> Either String TelUpdates -> m [ BotMsg]
processUpdates' idStateMsg eitherUpdates = do
  case eitherUpdates of
    Left _ -> do
      throwError NotAnswer
    Right upd -> do
      print "processUpdates'"
      findLastMsgs' idStateMsg (result upd)


      

findLastMsgs' :: TelMonad r m =>  Int -> [TelUpdate] -> m [ BotMsg]
findLastMsgs' _ [] = return []
findLastMsgs' lastId arr = do
  let u = map (findMsg lastId)  arr
  let u' = mapMaybe' u

  return u'





processUpdates :: TelMonad r m => Int -> Either String TelUpdates -> m [BotMsg]
processUpdates idStateMsg eitherUpdates = do
  case eitherUpdates of
    Left _ -> do
      throwError NotAnswer
    Right upd -> do
      print "processUpdates"
      findLastMsgs idStateMsg (result upd)
      

findLastMsgs :: TelMonad r m =>  Int -> [TelUpdate] -> m [BotMsg]
findLastMsgs _ [] = return []
findLastMsgs lastId arr = do
  let u = mapMaybe (findMsg lastId)  arr
  print $ length  u
  return u

findMsg :: Int -> TelUpdate ->   Maybe BotMsg
findMsg lastId telUpd = if lastId < idMsgO
                            then  
                              Just $ BotMsg (updateMsg telUpd) else Nothing
                        where
                           idMsgO = msgId $ updateMsg telUpd


mapMaybe' :: [Maybe a] -> [a]
mapMaybe' [] = []
mapMaybe' (Nothing:xs) =  mapMaybe' xs
mapMaybe' (Just a: xs) = [a] <> mapMaybe' xs

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
  