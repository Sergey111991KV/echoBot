module Adapter.VK.VKConfig where

import Adapter.VK.VKEntity (VKLongPollConfig)
import ClassyPrelude
    ( Show,
      Generic,
      Bool,
      Double,
      Int,
      String,
      MonadIO,
      Text,
      TVar,
      MonadReader )
  
import Data.Aeson (FromJSON)
import Data.Has (Has)
import Control.Monad.Except ( MonadError )
import Network.HTTP.Client ( Manager )

import Bot.Error ( Error )
import Log.ImportLog (Log, LogConfig)
import Adapter.VK.VKKeyboard ( VKKeyboard )

type VKMonad r m = (Has State r, MonadReader r m, MonadIO m, Log m, MonadError Error m)



data State = State
  { dynamicState :: TVar DynamicState
  , staticState :: StaticState
  }

data DynamicState =
  DynamicState
    { 
      longConfig :: VKLongPollConfig
    , lastMsgId :: Int
    , waitForRepeat :: Bool
    , repeats :: Int
    }
  deriving (Show, Generic)


data StaticState =
  StaticState
    {
      accessToken :: VKToken
    , helpMsg :: Text
    , version :: VKVersion
    , waits :: Int
    , getLongPollUrl :: String
    , getUpdatesUrl :: String
    , sendMsgUrl :: String
    , log :: LogConfig
    , vkManager :: Manager 
    , keyboard :: VKKeyboard
    }
  deriving Generic

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

