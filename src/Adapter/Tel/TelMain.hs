module Adapter.Tel.TelMain
  ( module Tel
  ) where

import Adapter.Tel.TelBot as Tel
  ( findLastMsg
  , getMsgLast
  , getNameAdapter
  , processUpdates
  , sendMsg
  , sendMsgHelp
  , sendText
  )
import Adapter.Tel.TelConfig as Tel
    ( State(..), TelMonad, StaticState(..), DynamicState(..) ) 
import Adapter.Tel.TelEchoBot as Tel
    ( sendMsgKeyboard,
      msgHelp,
      countRepeat,
      isWaitForRepeat,
      setWaitForRepeat,
      setCountRepeat,
      nameAdapter )
import Adapter.Tel.TelEntity as Tel
    ( telKeyb,
      TelButton(..),
      TelChat(..),
      TelKeyboard(..),
      TelKeyboardPostMessage(..),
      TelMsg(..),
      TelUpdate(..),
      TelUpdates(..),
      TelUser(..) )