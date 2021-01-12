module Adapter.VK.VKMain
  ( module VK
  ) where

import Adapter.VK.VKBot as VK
 
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

