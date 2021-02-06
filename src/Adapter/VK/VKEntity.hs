module Adapter.VK.VKEntity where


import ClassyPrelude
 
import Data.Aeson
import qualified Data.Vector as V

import Data.Scientific (Scientific(coefficient))
import Bot.Message

data UpdatesVK =
  UpdatesVK
    { tsCome :: Int
    , result :: [MessageVK]
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

instance FromJSON MessageVK where
  parseJSON (Array arr) = 
      if V.length arr == 7 && (parseValueInt (arr V.! 0) == 4) then do
      let x = parseValueInt $ arr V.! 0
      let y = parseValueInt $ arr V.! 1
      let e = parseValueInt $ arr V.! 2
      let r = parseValueInt $ arr V.! 3
      let t = parseValueInt $ arr V.! 4
      let i = parseValueText $ arr V.! 5
      let u = parseValueText $ arr V.! 6
      return $ MessageVK x y e r t i u
      else mzero
  parseJSON _ = mzero


parseValueInt :: Value -> Int
parseValueInt (Number a) = fromInteger $ coefficient a
parseValueInt _ = 0

parseValueText :: Value -> Text
parseValueText (String a) = a
parseValueText _ = "not parse"

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
