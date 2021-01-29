module Adapter.Tel.TelMain
  ( module Tel
  ) where

import Adapter.Tel.TelBot as Tel
  ( getLastMsgArray
  , sendMsg
  , sendMsgHelp
  , sendText
  )

import Adapter.Tel.TelConfig as Tel
  ( DynamicState(..)
  , State(..)
  , StaticState(..)
  , TelMonad
  )
import Adapter.Tel.TelEchoBot as Tel
  ( countRepeat
  , isWaitForRepeat
  , msgHelp
  , nameAdapter
  , sendMsgKeyboard
  , setCountRepeat
  , setWaitForRepeat
  )
import Adapter.Tel.TelEntity as Tel
  ( TelButton(..)
  , TelChat(..)
  , TelKeyboard(..)
  , TelKeyboardPostMessage(..)
  , TelMsg(..)
  , TelUpdate(..)
  , TelUpdates(..)
  , TelUser(..)
  , telKeyb
  )
