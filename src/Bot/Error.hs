module Bot.Error where

<<<<<<< HEAD
import ClassyPrelude (Eq, Generic, Ord, Read, Show(show), Text, ($), pack)

data Error
  = NotAnswer
  | NotConnect
=======
import ClassyPrelude
    ( ($), Eq, Ord, Read, Show(show), Generic, String, Text, pack )

data Error
  = NotAnswer
>>>>>>> master2
  | EmptyAnswer
  | NotNewMsg
  | CannotRepeatCountSet
  | CannotRepeatFalseNumber
<<<<<<< HEAD
  | CannotGetConfig
  | CannotGetMsg
=======
>>>>>>> master2
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
<<<<<<< HEAD
=======
  | ErrorParseConfig String 
>>>>>>> master2
  deriving (Eq, Ord, Read, Show, Generic)

errorText :: Error -> Text
errorText err = pack $ show err
