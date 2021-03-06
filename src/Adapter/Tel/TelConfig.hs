module Adapter.Tel.TelConfig where

import ClassyPrelude
    

import Control.Monad.Except ( MonadError )
import Data.Has (Has)
import Network.HTTP.Client ( Manager )

import Log.ImportLog ( LogConfig, Log ) 
import Bot.Error ( Error ) 
import Adapter.Tel.TelEntity
    ( TelKeyboard, TelegramLongPollParams ) 

type TelMonad r m = (Has  State r, MonadReader r m, MonadIO m, MonadError Error m, Log m)


data State = 
    State
        { dynamicState :: TVar DynamicState
        , staticState :: StaticState
        }  

data DynamicState =
  DynamicState
    { 
      repeats :: Int
    , lastMsgId :: Int
    , waitForRepeat :: Bool
    }
  deriving (Show, Generic)


data StaticState =
  StaticState
    {
      token :: String
    , textMsgHelp :: Text
    , botUrl :: String
    , getUpdates :: String
    , textSendMsgTel :: String
    , log :: LogConfig
    , telManager :: Manager 
    , telKeyboard :: TelKeyboard
    , delayTel :: TelegramLongPollParams
    } deriving Generic