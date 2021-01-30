module Adapter.VK.VKEntity where


import ClassyPrelude
 
import Data.Aeson
import qualified Data.Vector as V

import Data.Scientific (Scientific(coefficient))
import Bot.Message

data UpdatesVK =
  UpdatesVK
    { tsCome :: Int
    , result :: [Array] 
    }
  deriving (Show, Generic)

instance FromJSON UpdatesVK where
  parseJSON (Object v) = UpdatesVK <$> v .: "ts" <*> v .: "updates"
  parseJSON _ = mzero

  -- В общем здесь нужно в объекте получить массив массивов, а потом проверять его и на условия длины массивы 
  -- и первого элемента  - тогда можно будет просто убрать 2-3 функции за не надобностью

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

                -- have some problem here

-- instance FromJSON MessageVK where
--   parseJSON (Array a) = do
--     x <- parseJSON $ a V.! 0
--     y <- parseJSON $ a V.! 1
--     e <- parseJSON $ a V.! 2
--     r <- parseJSON $ a V.! 3
--     t <- parseJSON $ a V.! 4
--     i <- parseJSON $ a V.! 5
--     u <- parseJSON $ a V.! 6
--     return $ MessageVK x y e r t i u
--   parseJSON _ = mzero   
  

-- data UpdatesVK =
--   UpdatesVK
--     { tsCome :: Int
--     , result :: [MessageVK] 
--     }

                --- Poblem: I  not understand,  how to write condition array length == 7 and parse value
                ---             in parseJSON
          

parseArray :: Array -> [BotMsg]
parseArray arr =
  if V.length arr == 7 && (parseValueInt (arr V.! 0) == 4)
    then do
      let x = parseValueInt $ arr V.! 0
      let y = parseValueInt $ arr V.! 1
      let e = parseValueInt $ arr V.! 2
      let r = parseValueInt $ arr V.! 3
      let t = parseValueInt $ arr V.! 4
      let i = parseValueText $ arr V.! 5
      let u = parseValueText $ arr V.! 6
      [BotMsg (MessageVK x y e r t i u)]
    else []

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
