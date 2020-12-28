module Adapter.VK.VKKeyboard where

import ClassyPrelude
<<<<<<< HEAD
import Data.Aeson
=======
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
>>>>>>> master2
import qualified Data.ByteString.Lazy as B


jsonFile :: FilePath
jsonFile = "keyboard.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

<<<<<<< HEAD

=======
>>>>>>> master2
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
