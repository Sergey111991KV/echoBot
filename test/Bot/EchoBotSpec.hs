module Bot.EchoBotSpec where

import ClassyPrelude
import Test.Hspec
import Control.Monad.Except

import Bot.Bot
import Bot.EchoBot
import Bot.Error
import Bot.Message
import Log.ImportLog
import Log.LogMonad
import Fixture


data Fixture m =
  Fixture
    { _getLastMsgArray :: m [BotMsg]
    , _sendMsg :: BotMsg -> m ()
    , _sendHelpMsg :: Text -> BotMsg -> m ()
    , _writeLog :: LogWrite -> Text -> m ()
    , _writeLogE :: Text -> m ()
    , _writeLogW :: Text -> m ()
    , _writeLogD :: Text -> m ()
    , _msgHelp :: m Text
    , _countRepeat :: m Int
    , _isWaitForRepeat :: m Bool
    , _setWaitForRepeat :: Bool -> m ()
    , _setCountRepeat :: Int -> m ()
    , _sendMsgKeyboard :: BotMsg -> m  ()
    , _nameAdapter :: m Text
    }

    

newtype App a =
  App
    { unApp :: ReaderT (Fixture (Either Error)) (ExceptT Error IO) a
    }
  deriving (Applicative, Functor, Monad, MonadReader (Fixture (Either Error)), MonadIO, MonadError Error )

runApp :: Fixture (Either Error) -> App a -> IO (Either Error a)
runApp fixture app = do
   runExceptT $ runReaderT  (unApp  app) fixture


instance Bot App where
  getLastMsgArray =  asks _getLastMsgArray >>= liftEither
  sendMsg  msg = do
    func <- asks _sendMsg 
    liftEither $ func msg
  sendMsgHelp text msg = do
    func <- asks _sendHelpMsg 
    liftEither  $ func text msg


instance EchoBot App where
  msgHelp = asks _msgHelp >>= liftEither
  countRepeat = asks _countRepeat >>= liftEither
  isWaitForRepeat = asks _isWaitForRepeat >>= liftEither
  setWaitForRepeat isWait = do
    func <- asks _setWaitForRepeat 
    liftEither $ func isWait
  setCountRepeat count = do
    func <- asks _setCountRepeat
    liftEither $ func count
  sendMsgKeyboard msg = do
    func <- asks _sendMsgKeyboard
    liftEither $ func msg
  nameAdapter = asks _nameAdapter >>= liftEither


instance Log App where
  writeLog l txt = do
    time <- liftIO getCurrentTime
    liftIO $ writeLogHandler time logConfTest l txt
  writeLogD = writeLog Debug
  writeLogW = writeLog Warning
  writeLogE = writeLog Error

  
logConfTest :: LogConfig
logConfTest =
  LogConfig
    { logFile = "log-journalTest.txt"
    , logLevelForFile = Debug
    , logConsole = True
    }

emptyFixture :: Fixture m
emptyFixture =
  Fixture
    { _getLastMsgArray = unimplemented
    , _sendMsg = const unimplemented
    , _sendHelpMsg = const unimplemented
    , _writeLog = const unimplemented
    , _writeLogE = const unimplemented
    , _writeLogW = const unimplemented
    , _writeLogD = const unimplemented
    , _msgHelp = unimplemented
    , _countRepeat = unimplemented
    , _isWaitForRepeat = unimplemented
    , _setWaitForRepeat = const unimplemented
    , _setCountRepeat = const unimplemented
    , _sendMsgKeyboard = const unimplemented
    , _nameAdapter = unimplemented
    }


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
  describe "sendMsgEcho message" $ do
    it "should send EchoMessage for countRepeat" $ do
      let fixture =
            emptyFixture
              { _countRepeat = return 1
              , _sendMsg = \_ -> return  ()
              , _nameAdapter = return "Test"
              }
      runApp fixture (sendMsgEcho (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
        Right ()
--     -- it "should not send EchoMessage for countRepeat" $ do
--     --   let fixture =
--     --         emptyFixture
--     --           { _repeatsCount = return 1
--     --           , _sendMsg = \_ -> return ()
--     --           , _nameAdapter = return "Test"
--     --           }
--     --   runApp fixture (sendMsgEcho (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
--     --     Left EmptyAnswer
--   -- describe "sendMsgKeyboard send" $ do
--   --   it "should send sendMsgKeyboard " $ do
--   --     let fixture =
--   --           emptyFixture
--   --             { _sendKeyboardMsg = \_ -> return ()
--   --             , _nameAdapter = return "Test"
--   --             , _setWaitingForRepeats = \_ -> return ()
--   --             }
--   --     runApp fixture (processEchoBot (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
--   --       Right ()
--   --   it "should not send sendMsgKeyboard just setWaitForRepeat is Error" $ do
--   --     let fixture =
--   --           emptyFixture
--   --             { _sendKeyboardMsg = \_ -> return ()
--   --             -- CannotSendMsg
--   --             , _nameAdapter = return "Test"
--   --             , _setWaitingForRepeats = \_ -> return ()
--   --             }
--   --     runApp fixture (processEchoBot (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
--   --       Left CannotSendMsg
--   --   it "should not send sendMsgKeyboard just sendMsgKeyboard is Error" $ do
--   --     let fixture =
--   --           emptyFixture
--   --             { _sendKeyboardMsg = \_ -> return ()
--   --             -- $ Left EmptyAnswer
--   --             , _nameAdapter = return "Test"
--   --             , _setWaitingForRepeats = \_ -> return ()
--   --             }
--   --     runApp fixture (processEchoBot (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
--   --       Left EmptyAnswer
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
