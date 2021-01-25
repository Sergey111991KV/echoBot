module Adapter.Tel.TelEchoBot where

import ClassyPrelude

import Data.Has (Has(getter))

import Adapter.Tel.TelConfig
  ( DynamicState(repeats, waitForRepeat)
  , State(dynamicState, staticState)
  , StaticState(botUrl, telKeyboard, telManager, textMsgHelp,
            textSendMsgTel, token)
  , TelMonad
  )

import Adapter.Tel.TelEntity (TelKeyboardPostMessage(TelKeyboardPostMessage))
import Bot.Message (BotCompatibleMsg(chatId), BotMsg(..))
import Bot.Request (sendJSON')
import Log.ImportLog ( Log(writeLogD) )

sendMsgKeyboard :: TelMonad r m => BotMsg -> m ()
sendMsgKeyboard (BotMsg botMsg) = do
  writeLogD "sendMsgKeyboard Telegram" 
  st <- asks getter
  let idM = chatId botMsg
  let url =
        botUrl (staticState st) <>
        token (staticState st) <> "/" <> textSendMsgTel (staticState st)
  sendJSON'
    (telManager $ staticState st)
    url
    (TelKeyboardPostMessage
       idM
       "please select repeats count:"
       (telKeyboard (staticState st)))

msgHelp :: TelMonad r m => m Text
msgHelp = do
  writeLogD "msgHelp Telegram" 
  st <- asks getter
  return . textMsgHelp $ staticState st

countRepeat :: TelMonad r m => m Int
countRepeat = do
  writeLogD "countRepeat Telegram" 
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  return $ repeats dynSt

isWaitForRepeat :: TelMonad r m => m Bool
isWaitForRepeat = do
  writeLogD "isWaitForRepeat Telegram" 
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  return $ waitForRepeat dynSt

setWaitForRepeat :: TelMonad r m => Bool -> m ()
setWaitForRepeat boolWaitRepeat = do
  writeLogD "setWaitForRepeat Telegram" 
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  let newDynSt = dynSt {waitForRepeat = boolWaitRepeat}
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newDynSt
  return ()

setCountRepeat :: TelMonad r m => Int -> m ()
setCountRepeat countRep = do
  writeLogD "setCountRepeat Telegram" 
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  let newDynSt = dynSt {repeats = countRep}
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newDynSt
  return ()

nameAdapter :: TelMonad r m => m Text
nameAdapter = return " Telegram "
