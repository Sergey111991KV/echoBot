module Bot.Bot where


import Bot.Message (BotMsg)
import ClassyPrelude ( Text ) 
import Log.ImportLog ( Log ) 
import Control.Monad.Except ( MonadError )
import Bot.Error ( Error )

class (Log m, MonadError Error m ) =>
      Bot m
  where
  getMsgLast :: m BotMsg
  sendMsg :: BotMsg -> m ()
  sendMsgHelp :: Text -> BotMsg -> m ()


