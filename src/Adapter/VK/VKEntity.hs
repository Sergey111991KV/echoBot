module Adapter.VK.VKEntity where

import Bot.Message (BotCompatibleMessage(..))
import ClassyPrelude
  
import Data.Aeson
import qualified Data.Vector as V


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

instance FromJSON MessageVK where
  parseJSON (Array v) 
      | V.length v == 7 = do
            x <- parseJSON $ v V.! 0
            y <- parseJSON $ v V.! 1
            e <- parseJSON $ v V.! 0
            r <- parseJSON $ v V.! 1
            t <- parseJSON $ v V.! 0
            i <- parseJSON $ v V.! 1
            u <- parseJSON $ v V.! 0
            return $ MessageVK x y e r t i u
        | otherwise = mzero
  parseJSON _ = mzero


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
