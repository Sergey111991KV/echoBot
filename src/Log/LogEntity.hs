module Log.LogEntity where

import ClassyPrelude (Bool, Eq, FilePath, Generic, Ord, Read, Show)

import Data.Aeson (FromJSON, ToJSON)

type LogWriteInConfig = LogWrite

data LogConfig =
  LogConfig
    { logFile :: FilePath
    , logLevelForFile :: LogWriteInConfig
    , logConsole :: Bool
    }
  deriving (Show, Generic)

type LogForFile = LogWrite

data LogWrite
  = Debug
  | Warning
  | Error
  deriving (Eq, Ord, Read, Show, Generic)

instance ToJSON LogWrite

instance FromJSON LogWrite

instance ToJSON LogConfig

instance FromJSON LogConfig
