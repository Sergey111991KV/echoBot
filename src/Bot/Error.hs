module Bot.Error where

import ClassyPrelude
    ( ($), Eq, Ord, Read, Show(show), Generic, String, Text, pack )

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
  | HttpExceptionBot
  | ErrorDecodeData
  | CannotGetKeyboardVK
  | ErrorParseConfig String 
  deriving (Eq, Ord, Read, Show, Generic)

errorText :: Error -> Text
errorText err = pack $ show err