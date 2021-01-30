module Bot.BotSpec where

import Bot.Bot ( Bot(..) )
import Bot.Error ( Error(NotAnswer) )
import Bot.Message ( BotMsg(..), BotCompatibleMsg(..) )
import ClassyPrelude
    ( ($),
      Monad(return, (>>=)),
      Functor,
      Applicative,
      Bool(True),
      Int,
      IO,
      Either(..),
      MonadIO(..),
      const,
      Text,
      asks,
      getCurrentTime,
      MonadReader,
      ReaderT(..) )
import Log.ImportLog
    ( Log(..), LogConfig(..), LogWrite(..), writeLogHandler )
import Test.Hspec ( describe, it, shouldReturn, Spec )
import Control.Monad.Except
    ( Monad(return, (>>=)),
      Functor,
      MonadIO(..),
      ExceptT,
      runExceptT,
      MonadError(throwError),
      liftEither )
import Fixture ( unimplemented )


data Fixture m =
  Fixture
    { _getLastMsgArray :: m  [BotMsg]
    , _sendMsg :: BotMsg -> m ()
    , _sendHelpMsg :: Text -> BotMsg -> m  ()
    , _writeLog :: LogWrite -> Text -> m ()
    , _writeLogE :: Text -> m ()
    , _writeLogW :: Text -> m ()
    , _writeLogD :: Text -> m ()
    }



newtype App a =
  App
    { unApp :: ReaderT (Fixture (Either Error) ) (ExceptT Error IO) a
    }
  deriving (Applicative, Functor, Monad, MonadReader (Fixture (Either Error)), MonadIO, MonadError Error)

instance Bot App where
  getLastMsgArray =  asks _getLastMsgArray >>= liftEither
  sendMsg mess = do
    func <- asks _sendMsg  
    liftEither $ func mess
  sendMsgHelp text mess = do
    func <- asks _sendHelpMsg
    liftEither $ func text mess


logConfTest :: LogConfig
logConfTest =
  LogConfig
    { logFile = "log-journalTest.txt"
    , logLevelForFile = Debug
    , logConsole = True
    }


instance Log App where
  writeLog l txt = do
    time <- liftIO getCurrentTime
    liftIO $ writeLogHandler time logConfTest l txt
  writeLogD = writeLog Debug
  writeLogW = writeLog Warning
  writeLogE = writeLog Error



emptyFixture :: Fixture m
emptyFixture =
  Fixture
    { _getLastMsgArray = unimplemented
    , _sendMsg = const unimplemented
    , _sendHelpMsg = const unimplemented
    , _writeLog = const unimplemented
    , _writeLogE = unimplemented
    , _writeLogW = unimplemented
    , _writeLogD = unimplemented
    }

runApp :: Fixture (Either Error) -> App a -> IO (Either Error a)
runApp fixture app = do
   runExceptT $ runReaderT  (unApp  app) fixture
-- runApp fixture action = do
  -- flip runReaderT fixture . unApp $ action   ---- this text code I save for self-understand))

data EmptyMessage =
  EmptyMessage
    { textE :: Text
    , chatIdE :: Int
    , idMsgE :: Int
    }

instance BotCompatibleMsg EmptyMessage where
  textMsg = textE
  chatId = chatIdE
  idMsg = idMsgE

spec :: Spec
spec = do
  describe "sendMsg message" $ do
    let m = BotMsg (EmptyMessage "" 0 0)
    it "should send message " $ do
      let fixture =
            emptyFixture
              { _sendMsg = \_ -> do
                throwError NotAnswer
              } 
      runApp fixture (sendMsg (BotMsg (EmptyMessage "" 0 0))) `shouldReturn`
          Left NotAnswer
  describe "sendMsgHelp message" $ do
    let m = BotMsg (EmptyMessage "" 0 0)
    it "should send help message " $ do
      let fixture =
            emptyFixture
              { _sendHelpMsg = \_ _ -> return  ()
              }
      runApp fixture (sendMsgHelp "Test" (BotMsg (EmptyMessage "" 0 0))) `shouldReturn`
           Right ()
  