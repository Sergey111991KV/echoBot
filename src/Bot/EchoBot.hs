module Bot.EchoBot where

import ClassyPrelude
   
  
import Control.Monad.Except
    ( MonadError(..) )
import Control.Concurrent 

import Bot.Message
   
import Log.ImportLog (Log(writeLogD, writeLogE))
import Bot.Bot ( Bot(..) ) 
import Bot.Error
import Bot.Chats

class (Bot m ,MonadError Error m , MonadIO m)=>
      EchoBot m
  where
  msgHelp :: m Text
  countRepeat :: m Int
  isWaitForRepeat :: m Bool
  setWaitForRepeat :: Bool -> m ()
  setCountRepeat :: Int -> m ()
  sendMsgKeyboard :: BotMsg -> m ()
  nameAdapter :: m Text
  getChats :: m [Chat]

sendMsgEcho :: EchoBot m => BotMsg -> m ()
sendMsgEcho msg = do
  nameAd <- nameAdapter
  writeLogD $ "sendMsgEcho" <> nameAd
  repCount <- countRepeat
  msgCountRepeat repCount msg

msgCountRepeat :: EchoBot m => Int -> BotMsg -> m  ()
msgCountRepeat count meesBot = do
  nameAd <- nameAdapter
  writeLogD $ "msgCountRepeat" <> nameAd
  case count of
    0 -> do
      return  ()
    _ -> do
      sendMsg meesBot
      msgCountRepeat (count - 1) meesBot

tryGetCountRepeat :: EchoBot m => BotMsg -> m ()
tryGetCountRepeat (BotMsg msg) = do
  nameAd <- nameAdapter
  writeLogD $ "tryGetCountRepeat" <> nameAd
  let resultKeyboardAnswer = (readMay (textMsg msg) :: Maybe Int)
  case resultKeyboardAnswer of
    Nothing -> 
      throwError CannotRepeatCountSet
    Just newCount -> do
      if newCount <= 5 && newCount > 0
        then do
          setCountRepeat newCount
        else 
          throwError CannotRepeatFalseNumber 

handingBotMsg ::  EchoBot m => BotMsg -> m  ()
handingBotMsg (BotMsg msg) = do
    let txtMsg = textMsg msg
    isWait <- isWaitForRepeat
    if isWait
      then do
        case txtMsg of
          "/help" -> do
            helpMsg <- msgHelp
            sendMsgHelp helpMsg (BotMsg msg)
          "/repeat" -> do
            sendMsgKeyboard (BotMsg msg)
            setWaitForRepeat False
          _ -> do
            sendMsgEcho (BotMsg msg)
    else do
      tryGetCountRepeat (BotMsg msg)
      setWaitForRepeat True

handingBotMsgArray :: EchoBot m =>  [BotMsg] -> m  ()
handingBotMsgArray [] = return ()
handingBotMsgArray [x] = handingBotMsg x
handingBotMsgArray (x:xs) = do
  handingBotMsg x
  handingBotMsgArray xs


echoBot :: EchoBot m => m ()
echoBot = do
  arr <- getLastMsgArray
  handingBotMsgArray arr
  echoBot


