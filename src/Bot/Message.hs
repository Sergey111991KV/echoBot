module Bot.Message where

import ClassyPrelude

data BotMsg =
  forall a. BotCompatibleMsg a =>
            BotMsg a 

class BotCompatibleMsg a where
  textMsg :: a -> Text
  chatId :: a -> Int
  idMsg :: a -> Int
--   isEmpty :: a -> Bool

-- data EmptyMsg = EmptyMsg {
--   emptyTextMsg ::  Text
--   , emptyChatId ::  Int
--   , emptyIdMsg ::  Int
-- }

-- instance BotCompatibleMsg EmptyMsg where
--     textMsg = emptyTextMsg
--     chatId = emptyChatId
--     idMsg = emptyIdMsg
--     isEmpty _ = True

-- emptyMsg :: EmptyMsg
-- emptyMsg = EmptyMsg "" 0 0