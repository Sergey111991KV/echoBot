module Log.LogMonad where

import ClassyPrelude 
import Log.LogEntity (LogWrite)

class (Monad m, MonadIO m) =>
      Log m
  where
  writeLog :: LogWrite -> Text -> m ()
  writeLogE :: Text -> m ()
  writeLogW :: Text -> m ()
  writeLogD :: Text -> m ()
