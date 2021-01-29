module Adapter.VK.VKMain
  ( module VK
  ) where

import Adapter.VK.VKBot as VK
    ( caseOfGetMsg,
      getNewStateLongPool,
      getVKConfig,
      parseArrays,
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
import Adapter.VK.VKKeyboard as VK
