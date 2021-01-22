module Adapter.VK.VKBot where

import ClassyPrelude

import Control.Monad.Except (MonadError(throwError))
import Data.Aeson (Array, Value(Number, String), eitherDecode)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Has (Has(getter))
import Data.Scientific (Scientific(coefficient))
import qualified Data.Vector as V
import Network.HTTP.Client (Response(responseBody))

import Adapter.VK.VKConfig
  ( DynamicState(longConfig)
  , State(..)
  , StaticState(accessToken, getLongPollUrl, sendMsgUrl, version,
            vkManager, waits)
  , VKMonad
  , VKToken(takeVKToken)
  , VKVersion(takeVKVersion)
  )
import Adapter.VK.VKEntity
  ( MessageVK(MessageVK)
  , ResponseVK(ResponseVK)
  , UpdatesVK(UpdatesVK)
  , VKLongPollConfig(key, server, tsLast)
  )
import Bot.Error
import Bot.Request (sendReq, sendReq')

import Bot.Message

getLastMsgArray :: VKMonad r m => m [BotMsg]
getLastMsgArray = do
  (State dyn stat) <- getVKConfig
  stDyn <- readTVarIO dyn
  let url = "https://" <> server (longConfig stDyn)
  responseLastMsg <-
    sendReq
      (vkManager stat)
      url
      [ ("act", "a_check")
      , ("key", key (longConfig stDyn))
      , ("ts", show (tsLast $ longConfig stDyn))
      , ("wait", show (waits stat))
      ]
  caseOfGetMsg responseLastMsg

getVKConfig :: VKMonad r m => m State
getVKConfig = do
  st <- asks getter
  responseConfig <-
    sendReq
      (vkManager $ staticState st)
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
  -> m [BotMsg]
caseOfGetMsg responseGetMsg = do
  let upd =
        eitherDecode $ responseBody responseGetMsg :: Either String UpdatesVK
  case upd of
    Left _ -> do
      throwError CantConvertFromData
    Right (UpdatesVK ts arr) -> do
      setNewTs ts
      parseArrays arr

setNewTs :: VKMonad r m => Int -> m ()
setNewTs ts = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  let newDynSt = dynSt {longConfig = (longConfig dynSt) {tsLast = ts}}
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newDynSt
  return ()

parseArrays :: VKMonad r m => [Array] -> m [BotMsg]
parseArrays [] = return []
parseArrays arrays = return $ concatMap parseArray arrays

parseArray :: Array -> [BotMsg]
parseArray arr =
  if V.length arr == 7 && (parseValueInt (arr V.! 0) == 4)
    then do
      let x = parseValueInt $ arr V.! 0
      let y = parseValueInt $ arr V.! 1
      let e = parseValueInt $ arr V.! 2
      let r = parseValueInt $ arr V.! 3
      let t = parseValueInt $ arr V.! 4
      let i = parseValueText $ arr V.! 5
      let u = parseValueText $ arr V.! 6
      [BotMsg (MessageVK x y e r t i u)]
    else []

parseValueInt :: Value -> Int
parseValueInt (Number a) = fromInteger $ coefficient a
parseValueInt _ = 0

parseValueText :: Value -> Text
parseValueText (String a) = a
parseValueText _ = "not parse"

sendMsg :: VKMonad r m => BotMsg -> m ()
sendMsg (BotMsg msg) = do
  st <- asks getter
  sendReq'
    (vkManager $ staticState st)
    (sendMsgUrl $ staticState st)
    [ ("access_token", takeVKToken $ accessToken (staticState st))
    , ("v", show . takeVKVersion . version $ staticState st)
    , ("user_id", show $ chatId msg)
    , ("message", unpack $ textMsg msg)
    ]

sendMsgHelp :: VKMonad r m => Text -> BotMsg -> m ()
sendMsgHelp helpMess (BotMsg msg) = do
  st <- asks getter
  sendReq'
    (vkManager $ staticState st)
    (sendMsgUrl $ staticState st)
    [ ("access_token", takeVKToken $ accessToken (staticState st))
    , ("v", show . takeVKVersion . version $ staticState st)
    , ("user_id", show $ chatId msg)
    , ("message", unpack helpMess)
    ]

getNameAdapter :: VKMonad r m => m Text
getNameAdapter = return "VK"
