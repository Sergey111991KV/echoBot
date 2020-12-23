module Adapter.VK.VKKeyboard where

import ClassyPrelude
import Data.Aeson
import qualified Data.ByteString.Lazy as B


jsonFile :: FilePath
jsonFile = "keyboard.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile


data Keyboard =
  Keyboard
    { oneTime :: Bool
    , buttons :: [Array]
    , inline :: Bool
    }
  deriving (Show, Generic)

instance ToJSON Keyboard where
  toJSON (Keyboard ot btns inl) =
    object ["one_time" .= ot, "buttons" .= btns, "inline" .= inl]

instance FromJSON Keyboard where
  parseJSON (Object v) =
    Keyboard <$> v .: "one_time" <*> v .: "buttons" <*> v .: "inline"
  parseJSON _ = mzero
