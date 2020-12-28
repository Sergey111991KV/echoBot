module Lib
  ( startBot
  ) where

import qualified Adapter.Tel.TelMain as Tel
import qualified Adapter.VK.VKMain as VKBot
import Bot.Bot (Bot(..))
import Control.Monad.Except
    ( 
      ExceptT,
      MonadError(throwError),
      runExceptT )

import Bot.EchoBot ( finalEchoBot, EchoBot(..) )
import ClassyPrelude
    ( ($),
      Monad(return),
      Functor,
      Applicative,
      Semigroup((<>)),
      Bool(..),
      IO,
      Either(..),
      FilePath,
      String,
      Text,
      MonadIO(..),
      SomeException,
      (.),
      unless,
      getLine,
      print,
      null,
      asks,
      getCurrentTime,
      try,
      concurrently,
      newTVarIO,
      MonadReader,
      ReaderT(..) )
    
   
import Data.Has (Has(getter))
import Bot.Error
    ( errorText,
      Error(ErrorParseConfig, ErrorGetConfig, ErrorGetConfigPair) )
   
import qualified Config.Config as Config
import qualified Data.Text.IO as TIO
import Log.ImportLog (Log(..), LogWrite(Debug, Error, Warning), writeLogHandler)


newtype AppTel a =
  AppTel
    { unAppTel :: ReaderT Tel.State (ExceptT Error IO) a
    }
  deriving (Applicative, Functor, Monad, MonadReader Tel.State, MonadIO
  , MonadError Error 
  )

runTelegram :: Tel.State -> AppTel a -> IO (Either Error a)
runTelegram state app = do
  runExceptT $ runReaderT  (unAppTel  app) state


instance Log AppTel where
  writeLog l txt = do
    st <- asks getter 
    time <- liftIO getCurrentTime
    liftIO $ writeLogHandler time (Tel.log $ Tel.staticState st) l txt
  writeLogE = writeLog Error
  writeLogW = writeLog Warning
  writeLogD = writeLog Debug

instance Bot AppTel where
  getMsgLast = Tel.getMsgLast
  sendMsg = Tel.sendMsg
  sendMsgHelp = Tel.sendMsgHelp
 

instance EchoBot AppTel where
  msgHelp = Tel.msgHelp
  countRepeat = Tel.countRepeat
  isWaitForRepeat = Tel.isWaitForRepeat
  setWaitForRepeat = Tel.setWaitForRepeat
  setCountRepeat = Tel.setCountRepeat
  sendMsgKeyboard = Tel.sendMsgKeyboard
  nameAdapter = Tel.nameAdapter

runTelWithConfig :: Tel.State -> IO (Either Error ())
runTelWithConfig stateTelegram = do
  runTelegram stateTelegram finalEchoBot

getConfigTel ::  (MonadIO m, MonadError Error m) =>  FilePath -> m Tel.State
getConfigTel fp = do
  textFromFile <- liftIO . try $ TIO.readFile fp
  case (textFromFile :: Either SomeException Text) of
    Left _ -> throwError ErrorGetConfig
    Right configRaw -> do
      let parRaw = Config.getPairFromFile configRaw
      case parRaw of 
        Left _ -> throwError ErrorGetConfigPair
        Right ([], anotherString) -> do
          throwError $ ErrorParseConfig anotherString
        Right (configPair, anotherString) -> do
          unless  (null anotherString) . print $ ("This string has not been parsed:  " <> anotherString)
          dynSt <- Config.telDynamicConf configPair
          dynSt'  <- newTVarIO  dynSt
          staticSt <-  Config.telStaticConf configPair
          return $ Tel.State dynSt' staticSt

startTelegramBot :: FilePath -> IO ()
startTelegramBot fp = do
  caseOfConf <- runExceptT $ getConfigTel fp
  case caseOfConf of
    Left err -> do
      print $ errorText err <> " take right telegram bot options"
    Right conf -> do
      resultStart <- runTelWithConfig conf
      case resultStart of
        Left _ -> do
          print ("Check connection and put y/n for restart module telegram" :: String)
          restartProsess <- proccesInput
          if restartProsess
            then do
              startTelegramBot fp
            else do
              print ("Module Telegram is End" :: String)
        Right () -> do
          return ()

newtype AppVK a =
  AppVK
    { unAppVK :: ReaderT VKBot.State (ExceptT Error IO) a
    }
    deriving (Applicative, Functor, Monad, MonadReader VKBot.State, MonadIO
  , MonadError Error 
  )

runVK :: VKBot.State -> AppVK a -> IO (Either Error a)
runVK state app = do
  runExceptT $ runReaderT  (unAppVK  app) state

instance Log AppVK where
  writeLog l txt = do
    st <- asks getter 
    t <- liftIO getCurrentTime
    liftIO $ writeLogHandler t (VKBot.log $ VKBot.staticState st) l txt
  writeLogE = writeLog Error
  writeLogW = writeLog Warning
  writeLogD = writeLog Debug

instance Bot AppVK where
  getMsgLast = VKBot.getMsgLast
  sendMsg = VKBot.sendMsg
  sendMsgHelp = VKBot.sendMsgHelp


instance EchoBot AppVK where
  msgHelp = VKBot.msgHelp
  countRepeat = VKBot.countRepeat
  isWaitForRepeat = VKBot.isWaitForRepeat
  setWaitForRepeat = VKBot.setWaitForRepeat
  setCountRepeat = VKBot.setCountRepeat
  sendMsgKeyboard = VKBot.sendMsgKeyboard
  nameAdapter = VKBot.nameAdapter

runVKWithConfig :: VKBot.State -> IO (Either Error ())
runVKWithConfig stateVK = do
  runVK stateVK finalEchoBot

getConfigVK :: (MonadIO m, MonadError Error m) =>  FilePath -> m  VKBot.State
getConfigVK fp = do
  textFromFile <- liftIO . try $ TIO.readFile fp
  case (textFromFile :: Either SomeException Text) of
    Left _ -> throwError ErrorGetConfig
    Right configRaw -> do
      print configRaw
      let parRaw = Config.getPairFromFile configRaw
      case parRaw of 
        Left _ -> throwError ErrorGetConfigPair
        Right ([], anotherString) -> do
          throwError $ ErrorParseConfig anotherString
        Right (configPair,anotherString) -> do
          unless  (null anotherString) . print $ ("This string has not been parsed:  " <> anotherString)
          dynSt <- Config.vkDynamicConf configPair
          dynSt'  <- newTVarIO  dynSt
          staticSt <-  Config.vkStaticConf configPair
          return $ VKBot.State dynSt' staticSt 

 
startVKBot :: FilePath -> IO ()
startVKBot fp = do
  caseOfConf <- runExceptT $ getConfigVK fp
  case caseOfConf of
    Left err -> do
      print $ errorText err <> " take right vk bot options"
    Right conf -> do
      resultStart <- 
        runVKWithConfig conf
      case resultStart of
        Left _ -> do
          print ("Check connection and put y/n for restart module vk" :: String)
          restartProsess <- proccesInput
          if restartProsess
            then do
              startVKBot fp
            else do
              print ("Module VK is End" :: String)
        Right () -> do
          return ()

proccesInput :: IO Bool
proccesInput = do
  restartAnswer <- getLine
  case restartAnswer of
    "y" -> return True
    "n" -> return False
    _ -> do
      print  ("wrong input, please try input again - y or n !!!" :: String )
      proccesInput


startBot :: IO ()
startBot = do
  (_, _) <- 
    concurrently (startTelegramBot "bot.config") (startVKBot "bot.config")
  print ("End" :: String )