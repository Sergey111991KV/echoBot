module Config.VKKeyboard where

import ClassyPrelude
    ( Show,
      Applicative((<*>)),
      Generic,
      Bool,
      IO,
      FilePath,
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
import qualified Data.ByteString.Lazy as B


jsonFile :: FilePath
jsonFile = "keyboard.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

data VKKeyboard =
  VKKeyboard
    { oneTime :: Bool
    , buttons :: [Array]
    , inline :: Bool
    }
  deriving (Show, Generic)

instance ToJSON VKKeyboard where
  toJSON (VKKeyboard ot btns inl) =
    object ["one_time" .= ot, "buttons" .= btns, "inline" .= inl]

instance FromJSON VKKeyboard where
  parseJSON (Object v) =
    VKKeyboard <$> v .: "one_time" <*> v .: "buttons" <*> v .: "inline"
  parseJSON _ = mzero
