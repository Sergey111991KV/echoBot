module Bot.EchoBotSpec where

import ClassyPrelude
import Test.Hspec
import Control.Monad.Except

import Bot.Bot
import Bot.EchoBot
import Bot.Error
import Bot.Message
import Fixture
import Log.ImportLog
import Log.LogMonad

data Fixture m =
  Fixture
    { _getLastMsg :: m BotMsg
    , _sendMsg :: BotMsg -> m ()
    , _sendHelpMsg :: Text -> BotMsg -> m ()
    , _nameAdapter :: m Text
    , _logIn :: LogWrite -> Text -> m ()
    , _helpMsg :: m Text
    , _repeatsCount :: m Integer
    , _isWaitingForRepeats :: m Bool
    , _setWaitingForRepeats :: Bool -> m ()
    , _setRepeatsCount :: Integer -> m ()
    , _sendKeyboardMsg :: BotMsg -> m  ()
    }

newtype App a =
  App
    { unApp :: ReaderT (Fixture IO) (ExceptT Error IO) a
    }
  deriving (Applicative, Functor, Monad, MonadReader (Fixture IO), MonadIO, MonadError Error )

runApp :: Fixture IO -> App a -> IO (Either Error a)
runApp fixture app = do
   runExceptT $ runReaderT  (unApp  app) fixture

instance Bot App where
  getMsgLast = dispatch0 _getLastMsg
  sendMsg = dispatch _sendMsg
  sendMsgHelp = dispatch2 _sendHelpMsg


instance EchoBot App where
  msgHelp = dispatch0 _helpMsg
  countRepeat = dispatch0 _repeatsCount
  isWaitForRepeat = dispatch0 _isWaitingForRepeats
  setWaitForRepeat = dispatch _setWaitingForRepeats
  setCountRepeat = dispatch _setRepeatsCount
  sendMsgKeyboard = dispatch _sendKeyboardMsg
  nameAdapter = dispatch0 _nameAdapter

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
    { _getLastMsg = unimplemented
    , _sendMsg = const unimplemented
    , _sendHelpMsg = const unimplemented
    , _nameAdapter = unimplemented
    , _helpMsg = unimplemented
    , _repeatsCount = unimplemented
    , _isWaitingForRepeats = unimplemented
    , _setWaitingForRepeats = const unimplemented
    , _setRepeatsCount = const unimplemented
    , _sendKeyboardMsg = const unimplemented
    , _logIn = const unimplemented
    }

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
  describe "sendMsgEcho message" $ do
    it "should send EchoMessage for countRepeat" $ do
      let fixture =
            emptyFixture
              { _repeatsCount = return 1
              , _sendMsg = \_ -> return  ()
              , _nameAdapter = return "Test"
              }
      runApp fixture (sendMsgEcho (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
        Right ()
    it "should not send EchoMessage for countRepeat" $ do
      let fixture =
            emptyFixture
              { _repeatsCount = return 1
              , _sendMsg = \_ -> return ()
              , _nameAdapter = return "Test"
              }
      runApp fixture (sendMsgEcho (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
        Left EmptyAnswer
  -- describe "sendMsgKeyboard send" $ do
  --   it "should send sendMsgKeyboard " $ do
  --     let fixture =
  --           emptyFixture
  --             { _sendKeyboardMsg = \_ -> return ()
  --             , _nameAdapter = return "Test"
  --             , _setWaitingForRepeats = \_ -> return ()
  --             }
  --     runApp fixture (processEchoBot (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
  --       Right ()
  --   it "should not send sendMsgKeyboard just setWaitForRepeat is Error" $ do
  --     let fixture =
  --           emptyFixture
  --             { _sendKeyboardMsg = \_ -> return ()
  --             -- CannotSendMsg
  --             , _nameAdapter = return "Test"
  --             , _setWaitingForRepeats = \_ -> return ()
  --             }
  --     runApp fixture (processEchoBot (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
  --       Left CannotSendMsg
  --   it "should not send sendMsgKeyboard just sendMsgKeyboard is Error" $ do
  --     let fixture =
  --           emptyFixture
  --             { _sendKeyboardMsg = \_ -> return ()
  --             -- $ Left EmptyAnswer
  --             , _nameAdapter = return "Test"
  --             , _setWaitingForRepeats = \_ -> return ()
  --             }
  --     runApp fixture (processEchoBot (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
  --       Left EmptyAnswer
  -- describe "processEchoBot " $ do
  --   it "processEchoBot return right" $ do
  --     let fixture =
  --           emptyFixture
  --             { _repeatsCount = return 1
  --             , _sendMsg = \_ -> return  ()
  --             , _nameAdapter = return "Test"
  --             , _isWaitingForRepeats = return True
  --             , _sendHelpMsg = \_ _ -> return  ()
  --                   -- , _tryGetRepeatsCount = \_ -> return $ Right ()
  --             , _setWaitingForRepeats = \_ -> return ()
  --             }
  --     runApp fixture (processEchoBot (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
  --       Right ()
  --   it "processEchoBot return error just isWaitForRepeat is false" $ do
  --     let fixture =
  --           emptyFixture
  --             { _repeatsCount = return 1
  --             , _sendMsg = \_ -> return  ()
  --             , _nameAdapter = return "Test"
  --             , _isWaitingForRepeats = return False
  --             , _sendHelpMsg = \_ _ -> return  ()
  --                   -- , _tryGetRepeatsCount = \_ -> return $ Left EmptyAnswer
  --             , _setWaitingForRepeats = \_ -> return ()
  --             }
  --     runApp fixture (processEchoBot (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
  --       Left CannotRepeatCountSet
  --   it "processEchoBot return error just sendMsg is false" $ do
  --     let fixture =
  --           emptyFixture
  --             { _repeatsCount = return 1
  --             , _sendMsg = \_ -> return ()
  --             -- $ Left EmptyAnswer
  --             , _nameAdapter = return "Test"
  --             , _isWaitingForRepeats = return True
  --             , _sendHelpMsg = \_ _ -> return ()
  --                   -- , _tryGetRepeatsCount = \_ -> return $ Left EmptyAnswer
  --             , _setWaitingForRepeats = \_ -> return ()
  --             }
  --     runApp fixture (processEchoBot (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
  --       Left EmptyAnswer
  --   it
  --     "processEchoBot return error just isWaitForRepeat return false and tryGetCountRepeat is error" $ do
  --     let fixture =
  --           emptyFixture
  --             { _repeatsCount = return 1
  --             , _sendMsg = \_ -> return  ()
  --             , _nameAdapter = return "Test"
  --             , _isWaitingForRepeats = return False
  --             , _sendHelpMsg = \_ _ -> return  ()
  --                   -- , _tryGetRepeatsCount = \_ -> return $ Left EmptyAnswer
  --             , _setWaitingForRepeats = \_ -> return ()
  --             }
  --     runApp fixture (processEchoBot (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
  --       Left CannotRepeatCountSet
  --   it
  --     "processEchoBot return error just isWaitForRepeat return false and setWaitForRepeat is error" $ do
  --     let fixture =
  --           emptyFixture
  --             { _repeatsCount = return 1
  --             , _sendMsg = \_ -> return  ()
  --             , _nameAdapter = return "Test"
  --             , _isWaitingForRepeats = return False
  --             , _sendHelpMsg = \_ _ -> return  ()
  --                   -- , _tryGetRepeatsCount = \_ -> return $ Right ()
  --             , _setWaitingForRepeats = \_ -> return ()
  --             }
  --     runApp fixture (processEchoBot (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
  --       Left CannotRepeatCountSet
  --   it "processEchoBot return error if sendMsgHelp return error" $ do
  --     let fixture =
  --           emptyFixture
  --             { _repeatsCount = return 1
  --             , _sendMsg = \_ -> return  ()
  --             , _nameAdapter = return "Test"
  --             , _isWaitingForRepeats = return True
  --             , _sendHelpMsg = \_ _ -> return ()
  --             -- $ Left EmptyAnswer
  --             , _helpMsg = return "Help message"
  --                   -- , _tryGetRepeatsCount = \_ -> return $ Right ()
  --             , _setWaitingForRepeats = \_ -> return ()
  --             }
  --     runApp fixture (processEchoBot (BotMsg (EmptyMessage "/help" 0 1))) `shouldReturn`
  --       Left EmptyAnswer
  --   it
  --     "processEchoBot return error if message '/repeat' and setWaitForRepeat or sendMsgKeyboard return error " $ do
  --     let fixture =
  --           emptyFixture
  --             { _repeatsCount = return 1
  --             , _sendMsg = \_ -> return  ()
  --             , _nameAdapter = return "Test"
  --             , _isWaitingForRepeats = return True
  --             , _sendHelpMsg = \_ _ -> return ()
  --             -- $ Left EmptyAnswer
  --             , _helpMsg = return "Help message"
  --                   -- , _tryGetRepeatsCount = \_ -> return $ Right ()
  --             , _setWaitingForRepeats = \_ -> return ()
  --             , _sendKeyboardMsg = \_ -> return ()
  --             -- $ Left EmptyAnswer
  --             }
  --     runApp fixture (processEchoBot (BotMsg (EmptyMessage "/repeat" 0 1))) `shouldReturn`
  --       Left EmptyAnswer