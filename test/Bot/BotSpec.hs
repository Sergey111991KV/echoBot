module Bot.BotSpec where

import Bot.Bot
import Bot.Error
import Bot.Message
import ClassyPrelude
import Fixture
import Log.ImportLog
import Test.Hspec
import Control.Monad.Except

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

    -- undefined 
    -- dispatch0 _getLastMsgArray 
    -- здесь нужно правильно определить тип, как я понял сюда добавить either or exeptT?
  sendMsg mess = asks _sendMsg mess >>= liftEither
    -- liftEither
    -- dispatch _sendMsg ---- здесь нужно правильно определить тип, как я понял сюда добавить either or exeptT?
  sendMsgHelp =  asks _sendMsgHelp >>= liftEither
    --  dispatch2 _sendHelpMsg ---- здесь нужно правильно определить тип, как я понял сюда добавить either or exeptT?


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
  -- flip runReaderT fixture . unApp $ action

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
                Left NotAnswer
    
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
  