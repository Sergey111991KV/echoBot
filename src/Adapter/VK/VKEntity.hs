module Adapter.VK.VKEntity where


import ClassyPrelude
 
import Data.Aeson
    ( (.:),
      object,
      FromJSON(parseJSON),
      Array,
      Value(Object),
      KeyValue((.=)),
      ToJSON(toJSON) )
      
import Bot.Message (BotCompatibleMsg(..))

data UpdatesVK =
  UpdatesVK
    { tsCome :: Int
    , result :: [Array] 
    }
  deriving (Show, Generic)

instance FromJSON UpdatesVK where
  parseJSON (Object v) = UpdatesVK <$> v .: "ts" <*> v .: "updates"
  parseJSON _ = mzero

data MessageVK =
  MessageVK
    { typeMessage :: Int
    , tsMess :: Int
    , flag :: Int
    , idFrom :: Int
    , unixtime :: Int
    , otherText :: Text
    , textVk :: Text
    }
  deriving (Show, Generic)

newtype ResponseVKSend =
  ResponseVKSend
    { responses :: Int
    }
  deriving (Show, Generic)

instance FromJSON ResponseVKSend where
  parseJSON (Object v) = ResponseVKSend <$> v .: "response"
  parseJSON _ = mzero

data VKLongPollConfig =
  VKLongPollConfig
    { key :: String
    , server :: String
    , tsLast :: Int
    }
  deriving (Show, Generic)

newtype ResponseVK =
  ResponseVK
    { response :: VKLongPollConfig
    }
  deriving (Show, Generic)

instance FromJSON ResponseVK

instance FromJSON VKLongPollConfig where
  parseJSON (Object v) =
    VKLongPollConfig <$> v .: "key" <*> v .: "server" <*> v .: "ts"
  parseJSON _ = mzero

instance BotCompatibleMsg MessageVK where
  textMsg m = textVk m
  chatId m = idFrom m
  idMsg m = tsMess m
  -- isEmpty _ = False

data VKPostMessage =
  VKPostMessage
    { vkUserId :: Int
    , vkMsg :: Text
    , vkToken :: String
    , vkVersion :: Double
    }
  deriving (Show, Generic)

instance ToJSON VKPostMessage where
  toJSON VKPostMessage {..} =
    object
      [ "user_id" .= vkUserId
      , "message" .= vkMsg
      , "access_token" .= vkToken
      , "v" .= vkVersion
      ]
