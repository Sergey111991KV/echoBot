module Main where

import Lib
import Data.Aeson
import Data.Has (Has(getter))



newtype AppTel a =
  AppTel
    { unAppTel :: ReaderT String (ExceptT Error IO) a
    }
  deriving (Applicative, Functor, Monad, MonadReader String, MonadIO
  , MonadError Error 
  )

runTelegram :: String -> AppTel a -> IO (Either Error a)
runTelegram state app = do
  runExceptT $ runReaderT  (unAppTel  app) state








main :: IO ()
main = startBot
