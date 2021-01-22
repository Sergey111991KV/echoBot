module Adapter.VK.VKEchoBot where

import ClassyPrelude

import Data.Aeson (encode)
import Data.Has (Has(getter))

import Adapter.VK.VKConfig
  ( DynamicState(repeats, waitForRepeat)
  , State(dynamicState, staticState)
  , StaticState(accessToken, helpMsg, keyboard, sendMsgUrl, version,
            vkManager)
  , VKMonad
  , VKToken(takeVKToken)
  , VKVersion(takeVKVersion)
  )
import Bot.Message (BotCompatibleMsg(chatId, textMsg), BotMsg(..))
import Bot.Request (sendReq')

sendMsgKeyboard :: VKMonad r m => BotMsg -> m ()
sendMsgKeyboard (BotMsg msg) = do
  st <- asks getter
  sendReq'
    (vkManager $ staticState st)
    (sendMsgUrl $ staticState st)
    [ ("access_token", takeVKToken $ accessToken (staticState st))
    , ("v", show . takeVKVersion . version $ staticState st)
    , ("user_id", show $ chatId msg)
    , ("message", unpack $ textMsg msg)
    , ("keyboard", unpack $ decodeUtf8 (encode . keyboard $ staticState st))
    ]

msgHelp :: VKMonad r m => m Text
msgHelp = do
  st <- asks getter
  return . helpMsg $ staticState st

countRepeat :: VKMonad r m => m Int
countRepeat = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
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

setCountRepeat :: VKMonad r m => Int -> m ()
setCountRepeat count = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  let newStDyn = dynSt {repeats = count}
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newStDyn
  return ()

nameAdapter :: VKMonad r m => m Text
nameAdapter = return " VK "
