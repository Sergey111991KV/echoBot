module Adapter.VK.VKEchoBot where

import ClassyPrelude
    ( ($),
      Monad(return),
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
      StaticState(vkManager, accessToken, version, helpMsg),
      VKMonad,
      VKToken(takeVKToken),
      VKVersion(takeVKVersion) )


sendMsgKeyboard :: VKMonad r m => BotMsg -> m ()
sendMsgKeyboard (BotMsg msg) =  do
  st <- asks getter 
  keyKeyboard <- liftIO  getJSON
  let keyboardMaybe =  (decode keyKeyboard :: Maybe Keyboard)
  case keyboardMaybe of
    Nothing -> do
      throwError CannotSendKeyboard
    Just k  -> do
      let url = "https://api.vk.com/method/messages.send"
      liftIO $ sendRequestUrl (vkManager $ staticState st) url
       [    ("access_token", takeVKToken $ accessToken (staticState st))
          , ("v", show . takeVKVersion . version $ staticState st)
          , ("user_id", show $ chatId msg)
          , ("message" , unpack $ textMsg msg)
          ,  ("keyboard",unpack $ decodeUtf8 (encode k))
        ]



-- "https://api.vk.com/method/messages.send?access_token=1e3ca5da18082a12066e5c544f0de472e2bcc8d6c774ad2a79754ecacff5bdb7573c3dd9062a6dbc8bd05&
-- v=5.52&user_id=442266618&message=%2Frepeat&keyboard=%7B%22buttons%22%3A%5B%5B%7B%22color%22%3A%22positive%22%2C%
-- 22action%22%3A%7B%22payload%22%3A%22%7B%5C%22button%5C%22%3A%20%5C%221%5C%22%7D%22%2C%22type%22%3A%22text%22%2C%
-- 2label%22%3A%221%22%7D%7D%2C%7B%22color%22%3A%22positive%22%2C%22action%22%3A%7B%22payload%22%3A%22%7B%5C%22button%
-- 5C%22%3A%20%5C%222%5C%22%7D%22%2C%22type%22%3A%22text%22%2C%22label%22%3A%222%22%7D%7D%5D%2C%5B%7B%22color%22%3A%22
-- positive%22%2C%22action%22%3A%7B%22payload%22%3A%22%7B%5C%22button%5C%22%3A%20%5C%223%5C%22%7D%22%2C%22type%22%3A%22text%
-- 22%2C%22label%22%3A%223%22%7D%7D%2C%7B%22color%22%3A%22positive%22%2C%22action%22%3A%7B%22payload%22%3A%22%7B%5C%22button%5
-- C%22%3A%20%5C%224%5C%22%7D%22%2C%22type%22%3A%22text%22%2C%22label%22%3A%224%22%7D%7D%2C%7B%22color%22%3A%22positive%22%2C%22a
-- ction%22%3A%7B%22payload%22%3A%22%7B%5C%22button%5C%22%3A%20%5C%225%5C%22%7D%22%2C%22type%22%3A%22text%22%2C%22label%22%3A%225%
-- 22%7D%7D%5D%5D%2C%22inline%22%3Atrue%2C%22one_time%22%3Afalse%7D" - это запрос клавиатуры - я думаю это не нормально



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