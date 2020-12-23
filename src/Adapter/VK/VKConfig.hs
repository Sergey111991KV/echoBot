module Adapter.VK.VKConfig where

import Adapter.VK.VKEntity (VKLongPollConfig)
import ClassyPrelude
  ( Bool
  , Double
  , Generic
  , Integer
  , MonadIO
  , MonadReader
  , Show
  , String
  , TVar
  , Text
  )
import Data.Aeson (FromJSON)
import Data.Has (Has)
import Control.Monad.Except ( MonadError )

import Bot.Error ( Error )
import Log.ImportLog (Log, LogConfig)

type VKMonad r m = (Has State r, MonadReader r m, MonadIO m, Log m, MonadError Error m)



data State = State
  { dynamicState :: TVar DynamicState
  , staticState :: StaticState
  }

data DynamicState =
  DynamicState
    { 
      longConfig :: VKLongPollConfig
    , lastMsgId :: Integer
    , waitForRepeat :: Bool
    , repeats :: Integer
    }
  deriving (Show, Generic)


data StaticState =
  StaticState
    {
      accessToken :: VKToken
    , helpMsg :: Text
    , version :: VKVersion
    , waits :: Integer
    , getLongPollUrl :: String
    , getUpdatesUrl :: String
    , sendMsgUrl :: String
    , log :: LogConfig
    }
  deriving (Show, Generic)

newtype VKToken =
  VKToken
    { takeVKToken :: String
    }
  deriving (Show, Generic)

instance FromJSON VKToken

newtype VKVersion =
  VKVersion
    { takeVKVersion :: Double
    }
  deriving (Show, Generic)

instance FromJSON VKVersion

newtype VKUrl =
  VKUrl
    { takeVKUrl :: Text
    }
  deriving (Show, Generic)

instance FromJSON VKUrl

