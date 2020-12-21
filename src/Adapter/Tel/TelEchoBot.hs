module Adapter.Tel.TelEchoBot where

import ClassyPrelude
    ( ($),
      Monad(return),
      Semigroup((<>)),
      Bool,
      Integer,
      IO,
      String,
      Text,
      MonadIO(liftIO),
      (.),
      print,
      asks,
      swapTVar,
      atomically,
      readTVarIO )
import Control.Monad.Except ( MonadError(throwError) )
import Control.Exception ( catch )
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.Has (Has(getter))
import Network.HTTP.Client
    ( HttpException, Response(responseBody) )


import Adapter.Tel.TelConfig
    ( State(staticState, dynamicState),
      TelMonad,
      StaticState(botUrl, token, textSendMsgTel, textMsgHelp),
      DynamicState(waitForRepeat, repeats) )
import Adapter.Tel.TelEntity
    ( TelKeyboardPostMessage(TelKeyboardPostMessage), telKeyb )
import Bot.Error ( Error(HttpException) )
import Bot.Message (BotCompatibleMessage(chatId), BotMsg(..))
import Bot.Request

sendMsgKeyboard :: TelMonad r m => BotMsg -> m ()
sendMsgKeyboard (BotMsg botMsg) = do
  st <- asks getter
  let idM = chatId botMsg
  let url = botUrl (staticState st) <> token (staticState st) <> "/" <> textSendMsgTel (staticState st)
  upd <-
    liftIO . Control.Exception.catch (sendKeyboard idM url) $ \e -> do
      print (e :: HttpException)
      return "wrong"
  case upd of
    "wrong" -> throwError HttpException
    _ -> return ()

sendKeyboard :: Integer -> String -> IO LBS.ByteString
sendKeyboard chatIdKeyboard sendUrl = do
  res <-
    sendRequest'
      sendUrl
      (encode $
       TelKeyboardPostMessage
         chatIdKeyboard
         "please select repeats count:"
         telKeyb)
  return $ responseBody res

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
