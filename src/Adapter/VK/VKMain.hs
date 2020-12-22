module Adapter.VK.VKMain
  ( module VK
  ) where

import Adapter.VK.VKBot as VK
  ( caseOfGetMsg
  , getMsgLast
  , getNameAdapter
  , getNewStateLongPool
  , parseArray
  , parseArrays
  , parseValueInt
  , parseValueText
  , sendMsg
  , sendMsgHelp
  , setNewTs
  )
import Adapter.VK.VKConfig as VK
    ( VKUrl(..),
      VKVersion(..),
      VKToken(..),
      State(..),
      VKMonad,
      StaticState(..),
      DynamicState(..) )
  
import Adapter.VK.VKEchoBot as VK
    ( sendMsgKeyboard,
      msgHelp,
      countRepeat,
      isWaitForRepeat,
      setWaitForRepeat,
      setCountRepeat,
      nameAdapter )

import Adapter.VK.VKRequest as VK (sendVKKeyboard)
