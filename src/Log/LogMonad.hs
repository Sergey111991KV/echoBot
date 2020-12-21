module Log.LogMonad where

import ClassyPrelude (Monad, Text)

import Log.LogEntity (LogWrite)

class (Monad m) =>
      Log m
  where
  writeLog :: LogWrite -> Text -> m ()
  writeLogE :: Text -> m ()
  writeLogW :: Text -> m ()
  writeLogD :: Text -> m ()
