{-# OPTIONS_GHC -fno-warn-orphans #-}
module Adapter.VK.VKRequest
  ( sendVKKeyboard
  , msgSendVK
  , urlEncodeVars
  , sendRequest'
  , buildRequestBody
  ) where

import Adapter.VK.VKConfig
  ( VKToken(takeVKToken)
  , VKUrl(takeVKUrl)
  , VKVersion(takeVKVersion)
  )
import Adapter.VK.VKEntity (ResponseVKSend)
import ClassyPrelude
import Data.Aeson (Result, Value, encode, fromJSON)
import Network.HTTP.Req
  

import Bot.Request 
import Adapter.VK.VKKeyboard (Keyboard)


instance MonadHttp IO where
  handleHttpException = throwIO

sendVKKeyboard :: VKUrl -> Keyboard -> String -> VKVersion -> Integer -> IO ()
sendVKKeyboard url keyboard token version uiD =
  runReq defaultHttpConfig $ do
    r <-
      req
        POST
        (https (takeVKUrl url) /: "method" /: "messages.send")
        (ReqBodyUrlEnc $
         ("access_token" =: token) <>
         ("v" =: takeVKVersion version) <>
         ("user_id" =: uiD) <>
         ("message" =: ("keyboard" :: Text)) <>
         ("keyboard" =: decodeUtf8 (encode keyboard)))
        jsonResponse
        mempty
    liftIO . print . decodeUtf8 $ encode keyboard
    liftIO $ print (responseBody r :: Value)


msgSendVK ::
     VKUrl
  -> Text
  -> VKToken
  -> VKVersion
  -> Integer
  -> IO (Result ResponseVKSend)
msgSendVK url txtMsg token version uiD = do
  r <-
    req
      POST
      (https (takeVKUrl url) /: "method" /: "messages.send")
      (ReqBodyUrlEnc $
       ("access_token" =: takeVKToken token) <>
       ("v" =: takeVKVersion version) <>
       ("user_id" =: uiD) <> ("message" =: txtMsg))
      jsonResponse
      mempty
  let a = (fromJSON (responseBody r :: Value) :: Result ResponseVKSend)
  return a
