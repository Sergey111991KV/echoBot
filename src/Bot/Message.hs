module Bot.Message where

import ClassyPrelude ( Integer, Text )

data BotMsg =
  forall a. BotCompatibleMessage a =>
            BotMsg a 

class BotCompatibleMessage a where
  textMsg :: a -> Text
  chatId :: a -> Integer
  idMsg :: a -> Integer

