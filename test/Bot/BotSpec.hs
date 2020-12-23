module Bot.BotSpec where

import Bot.Bot
import Bot.Error
import Bot.Message
import ClassyPrelude
import Fixture
import Log.ImportLog
import Test.Hspec
import Control.Monad.Except
-- import Control.Monad.Error.Class

data Fixture m =
  Fixture
    { _getLastMsg :: m  BotMsg
    , _sendMsg :: BotMsg -> m ()
    , _sendHelpMsg :: Text -> BotMsg -> m  ()
    , _writeLog :: LogWrite -> Text -> m ()
    , _writeLogE :: Text -> m ()
    , _writeLogW :: Text -> m ()
    , _writeLogD :: Text -> m ()
    }



newtype App a =
  App
    { unApp :: ReaderT (Fixture IO) (ExceptT Error IO) a
    }
  deriving (Applicative, Functor, Monad, MonadReader (Fixture IO), MonadIO, MonadError Error)

instance Bot App where
  getMsgLast = dispatch0 _getLastMsg
  sendMsg = dispatch _sendMsg
  sendMsgHelp = dispatch2 _sendHelpMsg


logConfTest :: LogConfig
logConfTest =
  LogConfig
    { logFile = "log-journalTest.txt"
    , logLevelForFile = Debug
    , logConsole = True
    }
-- instance MonadError IOException IO

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
    { _getLastMsg = unimplemented
    , _sendMsg = const unimplemented
    , _sendHelpMsg = const unimplemented
    , _writeLog = const unimplemented
    , _writeLogE = unimplemented
    , _writeLogW = unimplemented
    , _writeLogD = unimplemented
    }

runApp :: Fixture IO -> App a -> IO (Either Error a)
runApp fixture app = do
   runExceptT $ runReaderT  (unApp  app) fixture
-- runApp fixture action = do
  -- flip runReaderT fixture . unApp $ action

data EmptyMessage =
  EmptyMessage
    { textE :: Text
    , chatIdE :: Integer
    , idMsgE :: Integer
    }

instance BotCompatibleMessage EmptyMessage where
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
                return  ()
              }
      runApp fixture (sendMsg (BotMsg (EmptyMessage "" 0 0))) `shouldReturn`
          Right ()
    it "should not send message " $ do
      let fixture =
            emptyFixture
              { _sendMsg = \_ -> do 
                return  NotAnswer
                --  throwError NotAnswer
              }
      runApp fixture (sendMsg (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
        Left EmptyAnswer
  describe "sendMsgHelp message" $ do
    let m = BotMsg (EmptyMessage "" 0 0)
    it "should send help message " $ do
      let fixture =
            emptyFixture
              { _sendHelpMsg = \_ _ -> return  ()
              }
      runApp fixture (sendMsgHelp "Test" (BotMsg (EmptyMessage "" 0 0))) `shouldReturn`
           Right ()
    it "should not send help message " $ do
      let fixture =
            emptyFixture
              { _sendHelpMsg = \_ _ -> 
                -- throwError NotAnswer
                return  ()
                -- throwError NotAnswer
              }
      runApp fixture (sendMsgHelp "Test" (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
         Left NotAnswer
