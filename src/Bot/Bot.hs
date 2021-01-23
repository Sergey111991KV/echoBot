module Bot.Bot where

import Bot.Error (Error)
import Bot.Message (BotMsg)
import ClassyPrelude 
import Control.Monad.Except (MonadError)
import Log.ImportLog (Log)

class (Log m, MonadError Error m) =>
      Bot m
  where
  getLastMsgArray :: m [BotMsg]
  sendMsg :: BotMsg -> m ()
  sendMsgHelp :: Text -> BotMsg -> m ()


