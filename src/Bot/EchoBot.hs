module Bot.EchoBot where

import ClassyPrelude
   
  
import Control.Monad.Except
    ( MonadError(..) )
import Control.Concurrent ( threadDelay )

import Bot.Message 
import Log.ImportLog (Log(writeLogD, writeLogE))
import Bot.Bot ( Bot(..) ) 
import Bot.Error
    ( Error(NotAnswer, CannotRepeatFalseNumber, NotNewMsg,
            CantConvertFromData, CantConvertFromArray, CannotRepeatCountSet),
      errorText )
   
   

class (Bot m ,MonadError Error m , MonadIO m)=>
      EchoBot m
  where
  msgHelp :: m Text
  countRepeat :: m Integer
  isWaitForRepeat :: m Bool
  setWaitForRepeat :: Bool -> m ()
  setCountRepeat :: Integer -> m ()
  sendMsgKeyboard :: BotMsg -> m ()
  nameAdapter :: m Text

sendMsgEcho :: EchoBot m => BotMsg -> m ()
sendMsgEcho msg = do
  repCount <- countRepeat
  msgCountRepeat repCount msg

msgCountRepeat :: EchoBot m => Integer -> BotMsg -> m  ()
msgCountRepeat count meesBot = do
  case count of
    0 -> do
      return  ()
    _ -> do
      sendMsg meesBot
      msgCountRepeat (count - 1) meesBot

tryGetCountRepeat :: EchoBot m => BotMsg -> m ()
tryGetCountRepeat (BotMsg msg) = do
  let resultKeyboardAnswer = (readMay (textMsg msg) :: Maybe Integer)
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
  if isEmpty msg then return () else do
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

handingBotMsgArray :: EchoBot m => [BotMsg] -> m  ()
handingBotMsgArray [] = return ()
handingBotMsgArray [x] = handingBotMsg x
handingBotMsgArray (x:xs) = do
  handingBotMsg x
  handingBotMsgArray xs



longPooll :: EchoBot m => m ()
longPooll = do
  arr <- getArrayMsgLast
  handingBotMsgArray arr
  longPooll


callback ::  EchoBot m => m  ()
callback = do
  -- nameA <- nameAdapter
  msg <- getMsgLast
  handingBotMsg msg 
  callback
      -- writeLogE $ errorText err <> nameA
      -- case err of
      --   NotNewMsg -> 
      --      liftIO (threadDelay 1000000) >> 
      --     callback
      --   CantConvertFromData -> 
      --     callback
      --   CantConvertFromArray ->  
      --     callback 
      --   CannotRepeatCountSet ->
      --     callback 
      --   NotAnswer ->
      --     throwError NotAnswer
      --   _ -> return ()
      


