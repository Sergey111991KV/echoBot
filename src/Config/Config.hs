module Config.Config  where

import ClassyPrelude
    ( ($),
      Eq((==)),
      Monad(return),
      Bool(True),
      Maybe(..),
      Either,
      String,
      Text,
      MonadIO(..),
      (||),
      fromMaybe,
      pack,
      IsMap(lookup) )
import Data.Aeson ( decode )
import Control.Monad.Except ( MonadError(throwError) ) 
import qualified Prelude as P
import qualified Text.Parsec as Pars
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings ) 

import qualified Adapter.Tel.TelMain as Tel
import qualified Adapter.VK.VKConfig as VKBot
import qualified Adapter.VK.VKEntity as VKBot
import Config.ParseConfig (ConfigPair, myParser)
import Log.ImportLog
  ( LogConfig(LogConfig, logConsole, logFile, logLevelForFile)
  , LogWrite(Debug)
  )
import Bot.Error ( Error(CannotGetKeyboardVK, ErrorGetConfigPair) ) 
import Adapter.VK.VKKeyboard ( getJSON, VKKeyboard )

getPairFromFile :: Text -> Either Pars.ParseError ([ConfigPair],String)
getPairFromFile = Pars.parse myParser ""

telDynamicConf :: MonadError Error m => [ConfigPair] -> m  Tel.DynamicState
telDynamicConf configPair = 
  if  lookup "RepeatCount" configPair == Nothing  
    then throwError ErrorGetConfigPair 
     else return $ Tel.DynamicState
    { 
      Tel.repeats = P.read $ fromMaybe "3" (lookup "RepeatCount" configPair)
    , Tel.lastMsgId = 0
    , Tel.waitForRepeat = True
    }

telStaticConf :: (MonadIO m, MonadError Error m)  => [ConfigPair] -> m Tel.StaticState
telStaticConf configPair = 
  if  lookup "TelegramToken" configPair == Nothing || 
      lookup "msgHelp" configPair == Nothing
    then throwError ErrorGetConfigPair
    else do
      manager <- liftIO $ newManager tlsManagerSettings
      return $  Tel.StaticState
        {
      Tel.token = fromMaybe "" (lookup "TelegramToken" configPair)
    , Tel.textMsgHelp = pack $ fromMaybe "msgHelp" (lookup "msgHelp" configPair)
    , Tel.botUrl  = "https://api.telegram.org/bot"
    , Tel.getUpdates = "getUpdates"
    , Tel.textSendMsgTel  = "sendMessage"
    , Tel.log =
                 LogConfig
                   { logFile = "log-journal.txt"
                   , logLevelForFile = Debug
                   , logConsole = True
                   }
    , Tel.telManager =  manager
    , Tel.telKeyboard = Tel.TelKeyboard
      { keyboard =
        [ [ Tel.TelButton "1" "1"
          , Tel.TelButton "2" "2"
          , Tel.TelButton "3" "3"
          , Tel.TelButton "4" "4"
          , Tel.TelButton "5" "5"
          ]
        ]
    , resize = True
    , oneTime = True
    }
    , Tel.delayTel = Tel.TelegramLongPollParams 1
      }
  
vkDynamicConf ::  MonadError Error m =>  [ConfigPair] -> m VKBot.DynamicState
vkDynamicConf  configPair = 
  if  lookup "RepeatCount" configPair == Nothing  
    then throwError ErrorGetConfigPair
    else return $ VKBot.DynamicState
              { VKBot.longConfig =
                  VKBot.VKLongPollConfig
                    {VKBot.server = "", VKBot.key = "", VKBot.tsLast = 0}
              , VKBot.lastMsgId = 0
              , VKBot.waitForRepeat = True 
              , VKBot.repeats =
                       P.read $ fromMaybe "3" (lookup "RepeatCount" configPair)
                   }

vkStaticConf ::  (MonadIO m, MonadError Error m) =>  [ConfigPair] ->  m  VKBot.StaticState
vkStaticConf  configPair = 
   if   paramToken == Nothing || 
        lookup "msgHelp" configPair == Nothing 
    then throwError ErrorGetConfigPair
    else do
      maybeKeyboard <-  liftIO  getJSON
      let resultMaybeKeyboard =  (decode maybeKeyboard :: Maybe VKKeyboard)
      case resultMaybeKeyboard of
        Nothing -> throwError CannotGetKeyboardVK
        Just vkKeyboard  -> do
          manager <- liftIO $ newManager tlsManagerSettings
          return $ VKBot.StaticState
              { VKBot.accessToken = VKBot.VKToken $ fromMaybe "" paramToken
              , VKBot.helpMsg =  pack $ fromMaybe "msgHelp" paramHelp
              , VKBot.version = VKBot.VKVersion 5.52
              , VKBot.waits = 25
              , VKBot.getLongPollUrl =
                 "https://api.vk.com/method/messages.getLongPollServer" 
             , VKBot.getUpdatesUrl = ""
             , VKBot.sendMsgUrl = "https://api.vk.com/method/messages.send"
             , VKBot.log =
                 LogConfig
                   { logFile = "log-journal.txt"
                   , logLevelForFile = Debug
                   , logConsole = True
                   }
            , VKBot.vkManager =  manager
            , VKBot.keyboard = vkKeyboard
             } 
  where
    paramToken = lookup "VKtoken" configPair
    paramHelp = lookup "msgHelp" configPair
 

