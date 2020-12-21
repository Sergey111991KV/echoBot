module Adapter.VK.VKEchoBot where

import ClassyPrelude
    ( ($),
      Monad(return),
      Bool,
      Integer,
      Maybe(Just, Nothing),
      Text,
      MonadIO(..),
      (.),
      asks,
      swapTVar,
      atomically,
      readTVarIO )
import Control.Monad.Except
    (  MonadError(throwError) )  
import Data.Has (Has(getter))

import Adapter.VK.VKConfig
    ( VKUrl(VKUrl),
      VKToken(takeVKToken),
      State(staticState, dynamicState),
      VKMonad,
      StaticState(accessToken, version, helpMsg),
      DynamicState(waitForRepeat, repeats) )
import qualified Log.ImportLog as Log
import Adapter.VK.VKRequest (getKeyButtons, sendVKKeyboard)
import Bot.Error ( Error(CannotSendKeyboard) ) 
import Bot.Message (BotCompatibleMessage(chatId), BotMsg(..))


sendMsgKeyboard :: (MonadIO m, VKMonad r m) => BotMsg -> m ()
sendMsgKeyboard (BotMsg msg) = do
  st <- asks getter 
  let keyKeyboard = getKeyButtons
  case keyKeyboard of
    Nothing -> do
      throwError CannotSendKeyboard
    Just k  -> do
      let url = "api.vk.com"
      _ <-
        liftIO $
        sendVKKeyboard
          (VKUrl url)
          k
          (takeVKToken . accessToken $ staticState st)
          (version $ staticState st)
          (chatId msg)
      Log.writeLogD "sendMsgKeyboard VK "
      return ()
       

msgHelp :: VKMonad r m => m Text
msgHelp = do
  st <- asks getter 
  -- Log.writeLogD "msgHelp VK "
  return . helpMsg $  staticState st

countRepeat :: VKMonad r m => m Integer
countRepeat = do
  st <- asks getter 
  dynSt <- readTVarIO $ dynamicState st 
  -- Log.writeLogD "countRepeat VK "
  return $ repeats dynSt


isWaitForRepeat :: VKMonad r m => m Bool
isWaitForRepeat = do
  st <- asks getter 
  dynSt <- readTVarIO $ dynamicState st 
  return $ waitForRepeat dynSt

setWaitForRepeat :: VKMonad r m => Bool -> m ()
setWaitForRepeat bl = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st 
  let newStDyn = dynSt {waitForRepeat = bl}
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newStDyn
  return ()

setCountRepeat :: VKMonad r m => Integer -> m ()
setCountRepeat count = do
  st <- asks getter 
  dynSt <- readTVarIO $ dynamicState st 
  let newStDyn = dynSt { repeats = count}
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newStDyn
  return ()

nameAdapter :: VKMonad r m => m Text
nameAdapter = return " VK "