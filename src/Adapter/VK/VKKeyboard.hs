module Adapter.VK.VKKeyboard where

import ClassyPrelude
    ( ($),
      Show,
      Applicative((<*>)),
      Generic,
      Bool,
      Maybe,
      IO,
      (<$>),
      MonadPlus(mzero),
      FilePath )

import Data.Aeson
    ( decode,
      (.:),
      object,
      FromJSON(parseJSON),
      Array,
      Value(Object),
      KeyValue((.=)),
      ToJSON(toJSON) )


import qualified Data.ByteString.Lazy as B
import System.IO.Unsafe (unsafePerformIO)

jsonFile :: FilePath
jsonFile = "keyboard.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

getKeyButtons :: Maybe Keyboard
getKeyButtons = decode $ unsafePerformIO getJSON

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
