module Adapter.VK.VKMain
  ( module VK
  ) where

import Adapter.VK.VKBot as VK
    ( caseOfGetMsg,
      getNameAdapter,
      getNewStateLongPool,
      getVKConfig,
      parseArray,
      parseArrays,
      parseValueInt,
      parseValueText,
      sendMsg,
      sendMsgHelp,
      setNewTs,
      getLastMsgArray )
import Adapter.VK.VKConfig as VK
    ( DynamicState(..),
      State(..),
      StaticState(..),
      VKMonad,
      VKToken(..),
      VKUrl(..),
      VKVersion(..) )
   
import Adapter.VK.VKEchoBot as VK
    ( sendMsgKeyboard,
      msgHelp,
      countRepeat,
      isWaitForRepeat,
      setWaitForRepeat,
      setCountRepeat,
      nameAdapter )

