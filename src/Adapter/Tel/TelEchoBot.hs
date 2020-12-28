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
<<<<<<< HEAD
    ( HttpException, Response(responseBody) )


import Adapter.Tel.TelConfig
    ( State(staticState, dynamicState),
      TelMonad,
      StaticState(botUrl, token, textSendMsgTel, textMsgHelp),
      DynamicState(waitForRepeat, repeats) )
=======
    ( HttpException, Manager, Response(responseBody) )
  
import Adapter.Tel.TelConfig
    ( DynamicState(waitForRepeat, repeats),
      State(staticState, dynamicState),
      StaticState(botUrl, token, textSendMsgTel, telManager,
                  textMsgHelp),
      TelMonad )
   
>>>>>>> master2
import Adapter.Tel.TelEntity
    ( TelKeyboardPostMessage(TelKeyboardPostMessage), telKeyb )
import Bot.Error ( Error(HttpException) )
import Bot.Message (BotCompatibleMessage(chatId), BotMsg(..))
<<<<<<< HEAD
import Bot.Request
=======
import Bot.Request ( sendRequestWithJsonBody' )
>>>>>>> master2

sendMsgKeyboard :: TelMonad r m => BotMsg -> m ()
sendMsgKeyboard (BotMsg botMsg) = do
  st <- asks getter
  let idM = chatId botMsg
  let url = botUrl (staticState st) <> token (staticState st) <> "/" <> textSendMsgTel (staticState st)
  upd <-
<<<<<<< HEAD
    liftIO . Control.Exception.catch (sendKeyboard idM url) $ \e -> do
=======
    liftIO . Control.Exception.catch (sendKeyboard (telManager $ staticState st) idM url) $ \e -> do
>>>>>>> master2
      print (e :: HttpException)
      return "wrong"
  case upd of
    "wrong" -> throwError HttpException
    _ -> return ()

<<<<<<< HEAD
sendKeyboard :: Integer -> String -> IO LBS.ByteString
sendKeyboard chatIdKeyboard sendUrl = do
  res <-
    sendRequest'
=======
sendKeyboard :: Manager -> Integer -> String -> IO LBS.ByteString
sendKeyboard manager chatIdKeyboard sendUrl = do
  res <-
    liftIO $ sendRequestWithJsonBody'
      manager
>>>>>>> master2
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
