module Bot.Chats where

import ClassyPrelude

data Chat =
  forall a. BoChat a =>
            Chat a 

class BoChat a where
    isWait :: a -> Bool 
    numberRepeat :: a -> Int 
    idChat :: a -> Int
--   textMsg :: a -> Text
--   chatId :: a -> Int
--   idMsg :: a -> Int
--   isEmpty :: a -> Bool