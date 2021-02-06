module Adapter.VK.VKBot where

import ClassyPrelude
    ( ($),
      Monad(return),
      Functor(fmap),
      Show(show),
      Semigroup((<>)),
      Int,
      Either,
      String,
      MonadIO(liftIO),
      Text,
      (.),
      either,
      print,
      unpack,
      asks,
      swapTVar,
      atomically,
      readTVarIO )
    

import Control.Monad.Except (MonadError(throwError))
import Data.Aeson ( eitherDecode ) 
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Has (Has(getter))

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
    ( ResponseVK(ResponseVK),
      UpdatesVK(UpdatesVK),
      VKLongPollConfig(server, key, tsLast) )
 
import Bot.Error ( Error(CantConvertFromData, NotAnswer) )
import Bot.Request (sendReq, sendReq')
import Log.ImportLog ( Log(writeLogD) )
import Bot.Message
    ( BotCompatibleMsg(chatId, textMsg), BotMsg(..) )

getLastMsgArray :: VKMonad r m => m [BotMsg]
getLastMsgArray = do
  writeLogD "getLastMsgArray VK" 
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
  writeLogD "getVKConfig VK" 
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
  either (\_ -> throwError NotAnswer) getNewStateLongPool  upd


getNewStateLongPool :: VKMonad r m => ResponseVK -> m State
getNewStateLongPool (ResponseVK newLongPoll) = do
  writeLogD "getNewStateLongPool" 
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
  writeLogD "caseOfGetMsg VK" 
  let upd =
        eitherDecode $ responseBody responseGetMsg :: Either String UpdatesVK
  print upd

  either (\_ -> throwError CantConvertFromData) 
        (\ (UpdatesVK ts arr) -> do 
          setNewTs ts 
          return $ fmap BotMsg arr )  upd

setNewTs :: VKMonad r m => Int -> m ()
setNewTs ts = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  let newDynSt = dynSt {longConfig = (longConfig dynSt) {tsLast = ts}}
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newDynSt
  return ()


sendMsg :: VKMonad r m => BotMsg -> m ()
sendMsg (BotMsg msg) = do
  writeLogD "sendMsg VK" 
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
  writeLogD "sendMsgHelp VK" 
  st <- asks getter
  sendReq'
    (vkManager $ staticState st)
    (sendMsgUrl $ staticState st)
    [ ("access_token", takeVKToken $ accessToken (staticState st))
    , ("v", show . takeVKVersion . version $ staticState st)
    , ("user_id", show $ chatId msg)
    , ("message", unpack helpMess)
    ]

