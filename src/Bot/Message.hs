module Bot.Message where

import ClassyPrelude

data BotMsg =
  forall a. BotCompatibleMsg a =>
            BotMsg a 

class BotCompatibleMsg a where
  textMsg :: a -> Text
  chatId :: a -> Integer
  idMsg :: a -> Integer
  isEmpty :: a -> Bool

data EmptyMsg = EmptyMsg {
  emptyTextMsg ::  Text
  , emptyChatId ::  Integer
  , emptyIdMsg ::  Integer
}

instance BotCompatibleMsg EmptyMsg where
    textMsg = emptyTextMsg
    chatId = emptyChatId
    idMsg = emptyIdMsg
    isEmpty _ = True

emptyMsg :: EmptyMsg
emptyMsg = EmptyMsg "" 0 0