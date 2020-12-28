module Bot.Error where

import ClassyPrelude

data Error
  = NotAnswer
  | EmptyAnswer
  | NotNewMsg
  | CannotRepeatCountSet
  | CannotRepeatFalseNumber
  | CannotSendMsg
  | CannotSendMsgHelp
  | CannotSendKeyboard
  | CantConvertFromData
  | CantConvertFromArray
  | ErrorGetConfig
  | ErrorGetConfigPair
  | ErrorWorkConnection
  | ErrorGetConfigRight
  | ErrorGetConfigLeft
  | HttpException
  | ErrorParseConfig String 
  deriving (Eq, Ord, Read, Show, Generic)

errorText :: Error -> Text
errorText err = pack $ show err
