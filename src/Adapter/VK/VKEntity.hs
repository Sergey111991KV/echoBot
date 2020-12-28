module Adapter.VK.VKEntity where


import ClassyPrelude
<<<<<<< HEAD
import Data.Aeson
=======
    ( Show,
      Applicative((<*>)),
      Generic,
      Double,
      Integer,
      String,
      Text,
      (<$>),
      MonadPlus(mzero) )
import Data.Aeson
    ( (.:),
      object,
      FromJSON(parseJSON),
      Array,
      Value(Object),
      KeyValue((.=)),
      ToJSON(toJSON) )
      
>>>>>>> master2
import Bot.Message (BotCompatibleMessage(..))

data UpdatesVK =
  UpdatesVK
    { tsCome :: Integer
    , result :: [Array] 
    }
  deriving (Show, Generic)

instance FromJSON UpdatesVK where
  parseJSON (Object v) = UpdatesVK <$> v .: "ts" <*> v .: "updates"
  parseJSON _ = mzero

data MessageVK =
  MessageVK
    { typeMessage :: Integer
    , tsMess :: Integer
    , flag :: Integer
    , idFrom :: Integer
    , unixtime :: Integer
    , otherText :: Text
    , textVk :: Text
    }
  deriving (Show, Generic)

newtype ResponseVKSend =
  ResponseVKSend
    { responses :: Integer
    }
  deriving (Show, Generic)

instance FromJSON ResponseVKSend where
  parseJSON (Object v) = ResponseVKSend <$> v .: "response"
  parseJSON _ = mzero

data VKLongPollConfig =
  VKLongPollConfig
    { key :: String
    , server :: String
    , tsLast :: Integer
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

instance BotCompatibleMessage MessageVK where
  textMsg m = textVk m
  chatId m = idFrom m
  idMsg m = tsMess m

data VKPostMessage =
  VKPostMessage
    { vkUserId :: Integer
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
