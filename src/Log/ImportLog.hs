module Log.ImportLog
  ( module Y
  ) where

import Log.Log as Y (writeLogHandler)
import Log.LogEntity as Y
  ( LogConfig(..)
  , LogForFile
  , LogWrite(..)
  , LogWriteInConfig
  )
import Log.LogMonad as Y (Log(..))
