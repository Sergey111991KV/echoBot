<<<<<<< HEAD
module Config.Config where
=======
module Config.Config  where
>>>>>>> master2

import ClassyPrelude
    ( ($),
      Eq((==)),
      Monad(return),
      Bool(True),
      Maybe(Nothing),
      Either,
<<<<<<< HEAD
      (||),
      fromMaybe,
      Text,
      pack,
      IsMap(lookup) )
import Control.Monad.Except ( MonadError(throwError) ) 
import qualified Prelude as P
import qualified Text.Parsec as Pars

=======
      String,
      Text,
      MonadIO(..),
      (||),
      fromMaybe,
      pack,
      IsMap(lookup) )
  
import Control.Monad.Except ( MonadError(throwError) ) 
import qualified Prelude as P
import qualified Text.Parsec as Pars
import Network.HTTP.Client ( newManager )
import Network.HTTP.Client.TLS ( tlsManagerSettings ) 
>>>>>>> master2
import qualified Adapter.Tel.TelConfig as Tel
import qualified Adapter.VK.VKConfig as VKBot
import qualified Adapter.VK.VKEntity as VKBot
import Config.ParseConfig (ConfigPair, myParser)
import Log.ImportLog
  ( LogConfig(LogConfig, logConsole, logFile, logLevelForFile)
  , LogWrite(Debug)
  )
<<<<<<< HEAD
import Bot.Error

getPairFromFile :: Text -> Either Pars.ParseError [ConfigPair]
getPairFromFile = Pars.parse myParser ""

=======
import Bot.Error ( Error(ErrorGetConfigPair) )


getPairFromFile :: Text -> Either Pars.ParseError ([ConfigPair],String)
getPairFromFile = Pars.parse myParser ""
>>>>>>> master2

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

<<<<<<< HEAD
telStaticConf :: MonadError Error m => [ConfigPair] -> m Tel.StaticState
=======
telStaticConf :: (MonadIO m, MonadError Error m)  => [ConfigPair] -> m Tel.StaticState
>>>>>>> master2
telStaticConf configPair = 
  if  lookup "TelegramToken" configPair == Nothing || 
      lookup "msgHelp" configPair == Nothing
    then throwError ErrorGetConfigPair
<<<<<<< HEAD
    else return $  Tel.StaticState
    {
=======
    else do
      manager <- liftIO $ newManager tlsManagerSettings
      return $  Tel.StaticState
        {
>>>>>>> master2
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
<<<<<<< HEAD
    }
=======
    , Tel.telManager =  manager
      }
>>>>>>> master2
  
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

<<<<<<< HEAD
vkStaticConf ::  MonadError Error m =>  [ConfigPair] ->  m  VKBot.StaticState
=======
vkStaticConf ::  (MonadIO m, MonadError Error m) =>  [ConfigPair] ->  m  VKBot.StaticState
>>>>>>> master2
vkStaticConf  configPair = 
   if   lookup "VKtoken" configPair == Nothing || 
        lookup "msgHelp" configPair == Nothing
    then throwError ErrorGetConfigPair
<<<<<<< HEAD
    else return $ VKBot.StaticState
=======
    else do
      manager <- liftIO $ newManager tlsManagerSettings
      return $ VKBot.StaticState
>>>>>>> master2
              { VKBot.accessToken = VKBot.VKToken $ fromMaybe "" paramToken
              , VKBot.helpMsg =  pack $ fromMaybe "msgHelp" (lookup "msgHelp" configPair)
              , VKBot.version = VKBot.VKVersion 5.52
              , VKBot.waits = 25
              , VKBot.getLongPollUrl =
<<<<<<< HEAD
                 "https://api.vk.com/method/messages.getLongPollServer?"
             , VKBot.getUpdatesUrl = ""
             , VKBot.sendMsgUrl = ""
=======
                 "https://api.vk.com/method/messages.getLongPollServer?" 
             , VKBot.getUpdatesUrl = ""
             , VKBot.sendMsgUrl = "https://api.vk.com/method/messages.send"
>>>>>>> master2
             , VKBot.log =
                 LogConfig
                   { logFile = "log-journal.txt"
                   , logLevelForFile = Debug
                   , logConsole = True
                   }
<<<<<<< HEAD
=======
            , VKBot.vkManager =  manager
>>>>>>> master2
             } 
  where
    paramToken = lookup "VKtoken" configPair

