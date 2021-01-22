module Adapter.Tel.TelEntity where

import ClassyPrelude
  
import Data.Aeson
  ( FromJSON(parseJSON)
  , KeyValue((.=))
  , ToJSON(toJSON)
  , Value(Object)
  , (.:)
  , object
  , withObject
  )

import Bot.Message

data TelUpdates =
  TelUpdates
    { ok :: Bool
    , result :: [TelUpdate]
    }
  deriving (Show, Generic)

instance FromJSON TelUpdates where
  parseJSON =
    withObject "TelUpdates" $ \v -> TelUpdates <$> v .: "ok" <*> v .: "result"

data TelUpdate =
  TelUpdate
    { updateId :: Int
    , updateMsg :: TelMsg
    }
  deriving (Show, Generic)

instance FromJSON TelUpdate where
  parseJSON (Object v) = TelUpdate <$> v .: "update_id" <*> v .: "message"
  parseJSON _ = mzero

data TelMsg =
  TelMsg
    { msgId :: Int
    , from :: TelUser
    , chat :: TelChat
    , date :: Int
    , text :: String
    }
  deriving (Show, Generic)

instance FromJSON TelMsg where
  parseJSON (Object v) =
    TelMsg <$> v .: "message_id" <*> v .: "from" <*> v .: "chat" <*> v .: "date" <*>
    v .: "text"
  parseJSON _ = mzero

instance BotCompatibleMsg TelMsg where
  textMsg m = pack $ text m
  chatId m = chatIdTel $ chat m
  idMsg m = msgId m


data TelUser =
  TelUser
    { userId :: Int
    , firstName :: String
    }
  deriving (Show, Generic)

instance FromJSON TelUser where
  parseJSON (Object v) = TelUser <$> v .: "id" <*> v .: "first_name"
  parseJSON _ = mzero

data TelChat =
  TelChat
    { chatIdTel :: Int
    , chatFirstName :: String
    , chatType :: String
    }
  deriving (Show, Generic)

instance FromJSON TelChat where
  parseJSON (Object v) =
    TelChat <$> v .: "id" <*> v .: "first_name" <*> v .: "type"
  parseJSON _ = mzero

data TelKeyboardPostMessage =
  TelKeyboardPostMessage
    { tMsgChatId :: Int
    , tMsgText :: String
    , tKeyboardSend :: TelKeyboard
    }
  deriving (Show, Generic)

instance ToJSON TelKeyboardPostMessage where
  toJSON TelKeyboardPostMessage {..} =
    object
      [ "chat_id" .= tMsgChatId
      , "text" .= tMsgText
      , "reply_markup" .= tKeyboardSend
      ]

data TelKeyboard =
  TelKeyboard
    { keyboard :: [[TelButton]]
    , resize :: Bool
    , oneTime :: Bool
    }
  deriving (Show, Generic)

instance ToJSON TelKeyboard where
  toJSON (TelKeyboard kb res oT) =
    object
      ["keyboard" .= kb, "resize_keyboard" .= res, "one_time_keyboard" .= oT]

data TelButton =
  TelButton
    { tButtonText :: String
    , tButtonData :: String
    }
  deriving (Show, Generic)

instance ToJSON TelButton where
  toJSON TelButton {..} =
    object ["text" .= tButtonText, "callback_data" .= tButtonData]

telKeyb :: TelKeyboard
telKeyb =
  TelKeyboard
    { keyboard =
        [ [ TelButton "1" "1"
          , TelButton "2" "2"
          , TelButton "3" "3"
          , TelButton "4" "4"
          , TelButton "5" "5"
          ]
        ]
    , resize = True
    , oneTime = True
    }
