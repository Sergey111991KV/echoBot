module Adapter.VK.VKBot where

import ClassyPrelude
    ( ($),
      Eq((==)),
      Monad(return),
      Show(show),
      Semigroup((<>)),
      Int,
      Integer,
      Either(..),
      String,
      Text,
      MonadIO(liftIO),
      (.),
      (&&),
      unpack,
      asks,
      swapTVar,
      atomically,
      readTVarIO )

import Control.Concurrent ( threadDelay )
import Network.HTTP.Client ( Response(responseBody) )
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
import Bot.Request (sendReq, sendReq' )
import Bot.Error
    ( Error(CantConvertFromArray, NotAnswer, CantConvertFromData) )
import Bot.Message (BotCompatibleMessage(chatId, textMsg), BotMsg(..))


getMsgLast :: VKMonad r m => m BotMsg
getMsgLast = do
  (State dyn stat) <- getVKConfig
  stDyn <- readTVarIO dyn
  let url = "https://" <>  server (longConfig  stDyn)

  responseLastMsg <- sendReq (vkManager stat) url  [   ("act", "a_check")
                                                ,   ("key", key (longConfig  stDyn))
                                                ,   ("ts", show (tsLast $ longConfig  stDyn))
                                                ,   ("wait", show (waits  stat))
                                                ]
  caseOfGetMsg responseLastMsg


getVKConfig :: VKMonad r m => m  State
getVKConfig = do
  st <- asks getter
  responseConfig <- sendReq   (vkManager $ staticState st)
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

setNewTs :: VKMonad r m => Int  -> m  ()
setNewTs ts = do
  st <- asks getter
  dynSt <- readTVarIO $ dynamicState st
  let newDynSt = dynSt {longConfig =  (longConfig dynSt) {tsLast = ts} }
  _ <- liftIO . atomically $ swapTVar (dynamicState st) newDynSt
  return ()

-- использовать Array в чистом виде тоже не рекомендуется, у тебя где то еще это есть
-- но это не критично
parseArrays :: VKMonad r m => [Array] -> m BotMsg
parseArrays [] = do
  throwError CantConvertFromArray
  -- если список обновлений пустой выкидываем ошибку? почему?
  -- обновлений может и не быть если никто ничего не написал, это окей.
  -- т.к. parseArray рекурсивная, то в любом случае когда список заканчивается
  -- будет вычислятся этот кейс.
  --
  -- Т.о. ты пытаешься парсить элементы списка, первый который срабатывает
  -- (на условии if) ты и берешь за BotMsg. Только проблема в том что это список
  -- обновлений, обновлений может быть много, если много пользователей то это
  -- тысячи сообщений.
  --
  -- в телеге у тебя аналогично выбрасывает NotNewMsg, здесь, же сам понимаешь,
  -- CantConvertFromArray на NotNewMsg заменить не получится
parseArrays (x:xs) =
  if V.length x == 7 &&  (parseValueInt (x V.! 0) == 4) then parseArray x else parseArrays xs
-- а вообще тут лучше избавиться от рекурсии в пользу какойнибудь функции высшего порядка

-- лучше этому всетаки быть в инстансе FromJSON BotMsg
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
  liftIO (threadDelay 1000000) -- а здесь то задержка зачем?
  st <- asks getter
  -- env <- asks $ staticState . getter
  sendReq' (vkManager $ staticState st)
    (sendMsgUrl $ staticState st)
       [    ("access_token", takeVKToken $ accessToken (staticState st))
          , ("v", show . takeVKVersion . version $ staticState st)
          , ("user_id", show $ chatId msg)
          , ("message" , unpack $ textMsg msg) ]


sendMsgHelp :: VKMonad r m => Text -> BotMsg -> m  ()
sendMsgHelp helpMess (BotMsg msg) = do
  st <- asks getter
  sendReq' (vkManager $ staticState st)
    (sendMsgUrl $ staticState st)
       [    ("access_token", takeVKToken $ accessToken (staticState st))
          , ("v", show . takeVKVersion . version $ staticState st)
          , ("user_id", show $ chatId msg)
          , ("message" , unpack  helpMess) ]


getNameAdapter :: VKMonad r m => m Text
getNameAdapter = return "VK"
