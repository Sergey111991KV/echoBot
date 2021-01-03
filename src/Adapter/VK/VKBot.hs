module Adapter.VK.VKBot where

import ClassyPrelude
  
    
import Network.HTTP.Client
    ( httpLbs, parseRequest, Response(responseBody) ) 
import Data.Aeson ( eitherDecode, Array, Value(String, Number) )
import Control.Monad.Except
    ( MonadError(throwError) )
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Has (Has(getter))
import Data.Scientific (Scientific(coefficient))
import qualified Data.Vector as V


import Adapter.VK.VKConfig
    ( DynamicState(longConfig),
      State(..),
      StaticState(waits, getLongPollUrl, vkManager, sendMsgUrl,
                  accessToken, version),
      VKMonad,
      VKToken(takeVKToken),
      VKVersion(takeVKVersion) )
import Adapter.VK.VKEntity
  ( MessageVK(MessageVK)
  , ResponseVK(ResponseVK)
  , UpdatesVK(UpdatesVK)
  , VKLongPollConfig(key, server, tsLast)
  )
import Bot.Request 
import Bot.Error
    ( Error(CantConvertFromArray, NotAnswer, CantConvertFromData) )
import Bot.Message (BotCompatibleMessage(chatId, textMsg), BotMsg(..))


getMsgLast :: VKMonad r m => m BotMsg
getMsgLast = do
  (State dyn stat) <- getVKConfig 
  stDyn <- readTVarIO dyn
  let url = "https://" <>  server (longConfig  stDyn)
 
  responseLastMsg <- sendReqTest (vkManager stat) url  [   ("act", "a_check")
                                                ,   ("key", key (longConfig  stDyn))
                                                ,   ("ts", show (tsLast $ longConfig  stDyn))
                                                ,   ("wait", show (waits  stat))
                                                ]
  caseOfGetMsg responseLastMsg


                -- old version

  -- let url =
  --               "https://" <>
  --               server (longConfig  stDyn) <>
  --               "?act=a_check&key=" <>
  --               key (longConfig  stDyn) <>
  --               "&ts=" <>
  --               show (tsLast $ longConfig  stDyn) <>
  --               "&wait=" <> show (waits  stat)
  -- request <- liftIO $ parseRequest url
  -- responseLastMsg <- liftIO $ httpLbs request (vkManager stat)
  -- caseOfGetMsg responseLastMsg





getVKConfig :: VKMonad r m => m  State
getVKConfig = do
  st <- asks getter
  responseConfig <- sendReqTest   (vkManager $ staticState st) 
                (getLongPollUrl (staticState st))  
                [ ("access_token", takeVKToken . accessToken $ staticState st)
                , ("v", show . takeVKVersion . version $ staticState st)
                ]
  let upd =
        eitherDecode (responseBody responseConfig) :: Either String ResponseVK
  case upd of
    Left _ -> do
      throwError NotAnswer
    Right (ResponseVK vkconfigpoll) -> do
      getNewStateLongPool vkconfigpoll


              -- old version

--  let bodyReq =
--         urlEncodeVars
--           [ ("access_token", takeVKToken . accessToken $ staticState st)
--           , ("v", show . takeVKVersion . version $ staticState st)
--           ]
--   let url = getLongPollUrl (staticState st) <> bodyReq
--   request <- liftIO $ parseRequest url
--   responseConfig <- liftIO $ httpLbs request  (vkManager $ staticState st)
--   let upd =
--         eitherDecode (responseBody responseConfig) :: Either String ResponseVK
--   case upd of
--     Left _ -> do
--       throwError NotAnswer
--     Right (ResponseVK vkconfigpoll) -> do
--       getNewStateLongPool vkconfigpoll


  
getNewStateLongPool :: VKMonad r m => VKLongPollConfig -> m State
getNewStateLongPool newLongPoll = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  let newDynSt = dynSt {longConfig = newLongPoll}
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newDynSt
  asks getter
  
caseOfGetMsg ::
     VKMonad r m
  => Response Data.ByteString.Lazy.Internal.ByteString
  -> m BotMsg
caseOfGetMsg responseGetMsg = do
  let upd =
        eitherDecode $ responseBody responseGetMsg :: Either String UpdatesVK
  case upd of
    Left _ -> do
      throwError CantConvertFromData
    Right (UpdatesVK ts arr) -> do
      setNewTs ts
      parseArrays arr

setNewTs :: VKMonad r m => Integer -> m  ()
setNewTs ts = do
  st <- asks getter 
  dynSt <- readTVarIO $ dynamicState st 
  let newDynSt = dynSt {longConfig =  (longConfig dynSt) {tsLast = ts} }
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newDynSt
  return ()

parseArrays :: VKMonad r m => [Array] -> m BotMsg
parseArrays [] = do
  throwError CantConvertFromArray
parseArrays (x:xs) = 
  if V.length x == 7 &&  (parseValueInt (x V.! 0) == 4) then parseArray x else parseArrays xs

parseArray :: VKMonad r m => Array -> m BotMsg
parseArray arr = do
  let x = parseValueInt $ arr V.! 0
  let y = parseValueInt $ arr V.! 1
  let e = parseValueInt $ arr V.! 2
  let r = parseValueInt $ arr V.! 3
  let t = parseValueInt $ arr V.! 4
  let i = parseValueText $ arr V.! 5
  let u = parseValueText $ arr V.! 6
  return $ BotMsg (MessageVK x y e r t i u)

parseValueInt :: Value -> Integer
parseValueInt (Number a) = coefficient a
parseValueInt _ = 0

parseValueText :: Value -> Text
parseValueText (String a) = a
parseValueText _ = "not parse"

sendMsg :: VKMonad r m => BotMsg -> m ()
sendMsg (BotMsg msg) =  do
  st <- asks getter 
  sendReq' (vkManager $ staticState st)
    (sendMsgUrl $ staticState st)
       [    ("access_token", takeVKToken $ accessToken (staticState st))
          , ("v", show . takeVKVersion . version $ staticState st)
          , ("user_id", show $ chatId msg)
          , ("message" , unpack $ textMsg msg) ]

          -- old version 

  -- liftIO $ sendRequestUrl
  --   (vkManager $ staticState st)
  --   (sendMsgUrl $ staticState st)
  --      [    ("access_token", takeVKToken $ accessToken (staticState st))
  --         , ("v", show . takeVKVersion . version $ staticState st)
  --         , ("user_id", show $ chatId msg)
  --         , ("message" , unpack $ textMsg msg)
  --       ]




sendMsgHelp :: VKMonad r m => Text -> BotMsg -> m  ()
sendMsgHelp helpMess (BotMsg msg) = do
  st <- asks getter 
  liftIO $ sendRequestUrl
    (vkManager $ staticState st)
    (sendMsgUrl $ staticState st)
       [    ("access_token", takeVKToken $ accessToken (staticState st))
          , ("v", show . takeVKVersion . version $ staticState st)
          , ("user_id", show $ chatId msg)
          , ("message" , unpack  helpMess)
        ]

getNameAdapter :: VKMonad r m => m Text
getNameAdapter = return "VK"


    