module Bot.Error where

import ClassyPrelude (Eq, Generic, Ord, Read, Show(show), Text, ($), pack)

data Error
  = NotAnswer
  | NotConnect
  | EmptyAnswer
  | NotNewMsg
  | CannotRepeatCountSet
  | CannotRepeatFalseNumber
  | CannotGetConfig
  | CannotGetMsg
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
  deriving (Eq, Ord, Read, Show, Generic)

errorText :: Error -> Text
errorText err = pack $ show err
