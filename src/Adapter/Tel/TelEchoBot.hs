module Adapter.Tel.TelEchoBot where

import ClassyPrelude
    ( ($),
      Monad(return),
      Semigroup((<>)),
      Bool,
      Integer,
      Text,
      MonadIO(liftIO),
      (.),
      print,
      asks,
      swapTVar,
      atomically,
      readTVarIO )
import Data.Has (Has(getter))

 
  
import Adapter.Tel.TelConfig
    ( StaticState(botUrl, token, textSendMsgTel, telManager,
                  telKeyboard, textMsgHelp),
      DynamicState(waitForRepeat, repeats),
      State(staticState, dynamicState),
      TelMonad )
  
   
import Adapter.Tel.TelEntity
    ( TelKeyboardPostMessage(TelKeyboardPostMessage) )
import Bot.Message (BotCompatibleMessage(chatId), BotMsg(..))
import Bot.Request ( sendJSON' ) 

sendMsgKeyboard :: TelMonad r m => BotMsg -> m ()
sendMsgKeyboard (BotMsg botMsg) = do
  st <- asks getter
  let idM = chatId botMsg
  let url = botUrl (staticState st) <> token (staticState st) <> "/" <> textSendMsgTel (staticState st)
  sendJSON' (telManager $ staticState st) url (TelKeyboardPostMessage  
                                                idM
                                                "please select repeats count:" 
                                                (telKeyboard (staticState st)))

msgHelp :: TelMonad r m => m Text
msgHelp = do
  st <- asks getter
  return . textMsgHelp $ staticState st

countRepeat :: TelMonad r m => m Integer
countRepeat = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  return $ repeats dynSt
      

isWaitForRepeat :: TelMonad r m => m Bool
isWaitForRepeat = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  return $ waitForRepeat dynSt

setWaitForRepeat :: TelMonad r m => Bool -> m ()
setWaitForRepeat boolWaitRepeat = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  let newDynSt = dynSt {waitForRepeat = boolWaitRepeat}
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newDynSt
  _ <- liftIO $ readTVarIO (dynamicState st) --
  return ()

setCountRepeat :: TelMonad r m => Integer -> m ()
setCountRepeat countRep = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  print dynSt
  let newDynSt = dynSt  {repeats = countRep}
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newDynSt
  st' <-  asks getter
  dynSt' <- readTVarIO $ dynamicState st'
  print dynSt'
  return ()

nameAdapter :: TelMonad r m => m Text
nameAdapter = return " Telegram "
