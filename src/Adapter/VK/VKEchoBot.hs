module Adapter.VK.VKEchoBot where

import ClassyPrelude
    ( ($),
      Monad(return),
<<<<<<< HEAD
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
import Adapter.VK.VKKeyboard 
import Adapter.VK.VKConfig
    ( VKUrl(VKUrl),
      VKToken(takeVKToken),
      State(staticState, dynamicState),
      VKMonad,
      StaticState(accessToken, version, helpMsg),
      DynamicState(waitForRepeat, repeats) )
import qualified Log.ImportLog as Log
-- import Adapter.VK.VKRequest (getKeyButtons, sendVKKeyboard)
import Adapter.VK.VKRequest  
import Bot.Error ( Error(CannotSendKeyboard) ) 
import Bot.Message (BotCompatibleMessage(chatId), BotMsg(..))
import Data.Aeson


sendMsgKeyboard :: (MonadIO m, VKMonad r m) => BotMsg -> m ()
sendMsgKeyboard (BotMsg msg) = do
=======
      Show(show),
      Bool,
      Integer,
      Maybe(..),
      Text,
      MonadIO(liftIO),
      (.),
      unpack,
      asks,
      swapTVar,
      atomically,
      readTVarIO,
      Utf8(decodeUtf8) )

import Control.Monad.Except
    (  MonadError(throwError) )  
import Data.Has (Has(getter))
import Data.Aeson ( decode, encode )


import Bot.Error ( Error(CannotSendKeyboard) ) 
import Bot.Message
    ( BotCompatibleMessage(textMsg, chatId), BotMsg(..) )
import Bot.Request ( sendRequestUrl ) 
import Adapter.VK.VKKeyboard ( getJSON, Keyboard ) 
import Adapter.VK.VKConfig
    ( DynamicState(waitForRepeat, repeats),
      State(staticState, dynamicState),
      StaticState(vkManager, sendMsgUrl, accessToken, version, helpMsg),
      VKMonad,
      VKToken(takeVKToken),
      VKVersion(takeVKVersion) )
    


sendMsgKeyboard :: VKMonad r m => BotMsg -> m ()
sendMsgKeyboard (BotMsg msg) =  do
>>>>>>> master2
  st <- asks getter 
  keyKeyboard <- liftIO  getJSON
  let keyboardMaybe =  (decode keyKeyboard :: Maybe Keyboard)
  case keyboardMaybe of
    Nothing -> do
      throwError CannotSendKeyboard
    Just k  -> do
<<<<<<< HEAD
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
       
=======
      liftIO $ sendRequestUrl (vkManager $ staticState st) (sendMsgUrl $ staticState st)
       [    ("access_token", takeVKToken $ accessToken (staticState st))
          , ("v", show . takeVKVersion . version $ staticState st)
          , ("user_id", show $ chatId msg)
          , ("message" , unpack $ textMsg msg)
          ,  ("keyboard",unpack $ decodeUtf8 (encode k))
        ]
>>>>>>> master2

msgHelp :: VKMonad r m => m Text
msgHelp = do
  st <- asks getter 
<<<<<<< HEAD
  -- Log.writeLogD "msgHelp VK "
=======
>>>>>>> master2
  return . helpMsg $  staticState st

countRepeat :: VKMonad r m => m Integer
countRepeat = do
  st <- asks getter 
  dynSt <- readTVarIO $ dynamicState st 
<<<<<<< HEAD
  -- Log.writeLogD "countRepeat VK "
=======
>>>>>>> master2
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