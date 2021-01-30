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
    , _sendMsgHelp :: Text -> BotMsg -> m ()
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
    func <- asks _sendMsgHelp 
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
    , _sendMsgHelp = const unimplemented
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
    it "should not send EchoMessage for countRepeat" $ do
      let fixture =
            emptyFixture
              { _countRepeat = return 1
              , _sendMsg = \_ -> throwError CannotSendMsg
              , _nameAdapter = return "Test"
              }
      runApp fixture (sendMsgEcho (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
        Left CannotSendMsg

  describe "msgCountRepeat send" $ do
    it "should send msgCountRepeat " $ do
      let fixture =
            emptyFixture
              { _writeLogD = \_ -> return ()
              , _nameAdapter = return "Test"
              , _sendMsg = \_ -> return ()
              }
      runApp fixture (msgCountRepeat 1 (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
        Right ()
    it "should not send msgCountRepeat just setWaitForRepeat is Error" $ do
      let fixture =
            emptyFixture
              { _writeLogD = \_ -> return ()
              , _nameAdapter = return "Test"
              , _sendMsg = \_ -> throwError CannotSendMsg
              }
      runApp fixture (msgCountRepeat 1 (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
        Left CannotSendMsg

  describe "tryGetCountRepeat " $ do
    it "tryGetCountRepeat return right" $ do
      let fixture =
             emptyFixture
              { _writeLogD = \_ -> return ()
              , _writeLogE = \_  -> return ()
              , _nameAdapter = return "Test"
              , _sendMsg = \_ -> return ()
              , _setCountRepeat = \_ -> return ()
              }
      runApp fixture (tryGetCountRepeat (BotMsg (EmptyMessage "1" 0 1))) `shouldReturn`
        Right ()
    it "tryGetCountRepeat return error just setCountRepeat is false" $ do
      let fixture =
             emptyFixture
              { _writeLogD = \_ -> return ()
              , _writeLogE = \_  -> return ()
              , _nameAdapter = return "Test"
              , _sendMsg = \_ -> return ()
              , _setCountRepeat = \_ -> throwError CannotRepeatFalseNumber
              }
      runApp fixture (tryGetCountRepeat (BotMsg (EmptyMessage "1" 0 1))) `shouldReturn`
        Left CannotRepeatFalseNumber
    it "tryGetCountRepeat return error just wrong BotMessage text is false" $ do
      let fixture =
             emptyFixture
              { _writeLogD = \_ -> return ()
              , _writeLogE = \_  -> return ()
              , _nameAdapter = return "Test"
              , _sendMsg = \_ -> return ()
              }
      runApp fixture (tryGetCountRepeat (BotMsg (EmptyMessage "" 0 1))) `shouldReturn`
        Left CannotRepeatCountSet
  describe "handingBotMsg " $ do
    it "handingBotMsg return right" $ do
      let fixture =
             emptyFixture
            { _countRepeat = return 1
            , _isWaitForRepeat = return True
            , _setWaitForRepeat  = \ False -> return ()
            , _writeLogD = \_ -> return ()
            , _writeLogE = \_  -> return ()
            , _nameAdapter = return "Test"
            , _sendMsg = \_ -> return ()
            , _setCountRepeat = \_ -> return ()             
            }
      runApp fixture (handingBotMsg (BotMsg (EmptyMessage "Test" 0 1))) `shouldReturn`
        Right ()
  describe "handingBotMsg " $ do
    it "handingBotMsg return right with help message" $ do
      let fixture =
             emptyFixture
            { _countRepeat = return 1
            , _isWaitForRepeat = return True
            , _msgHelp = return $ "Help Test" 
            , _sendMsgHelp = \"Help Test" _ -> return ()
            , _setWaitForRepeat  = \ False -> return ()
            , _writeLogD = \_ -> return ()
            , _writeLogE = \_  -> return ()
            , _nameAdapter = return "Test"
            , _sendMsg = \_ -> return ()
            , _setCountRepeat = \_ -> return ()             
            }
      runApp fixture (handingBotMsg (BotMsg (EmptyMessage "/help" 0 1))) `shouldReturn`
        Right ()
  describe "handingBotMsg " $ do
    it "handingBotMsg return right with keyboard" $ do
      let fixture =
             emptyFixture
            { _countRepeat = return 1
            , _isWaitForRepeat = return True
            , _sendMsgKeyboard = \_ -> return ()
            , _setWaitForRepeat  = \ False -> return ()
            , _writeLogD = \_ -> return ()
            , _writeLogE = \_  -> return ()
            , _nameAdapter = return "Test"
            , _sendMsg = \_ -> return ()
            , _setCountRepeat = \_ -> return ()             
            }
      runApp fixture (handingBotMsg (BotMsg (EmptyMessage "/repeat" 0 1))) `shouldReturn`
        Right ()  

  describe "handingBotMsg " $ do
    it "handingBotMsg return error" $ do
      let fixture =
             emptyFixture
            { _countRepeat = return 1
            , _isWaitForRepeat = return True
            , _setWaitForRepeat  = \ False -> return ()
            , _writeLogD = \_ -> return ()
            , _writeLogE = \_  -> return ()
            , _nameAdapter = return "Test"
            , _sendMsg = \_ -> throwError CannotSendMsg
            , _setCountRepeat = \_ -> return ()             
            }
      runApp fixture (handingBotMsg (BotMsg (EmptyMessage "Test" 0 1))) `shouldReturn`
        Left CannotSendMsg
  describe "handingBotMsg " $ do
    it "handingBotMsg return error with help message" $ do
      let fixture =
             emptyFixture
            { _countRepeat = return 1
            , _isWaitForRepeat = return True
            , _msgHelp = return  "Help Test" 
            , _sendMsgHelp = \_ _ -> throwError CannotSendMsgHelp
            , _setWaitForRepeat  = \ False -> return ()
            , _writeLogD = \_ -> return ()
            , _writeLogE = \_  -> return ()
            , _nameAdapter = return "Test"
            , _sendMsg = \_ -> return ()
            , _setCountRepeat = \_ -> return ()             
            }
      runApp fixture (handingBotMsg (BotMsg (EmptyMessage "/help" 0 1))) `shouldReturn`
        Left CannotSendMsgHelp
  describe "handingBotMsg " $ do
    it "handingBotMsg return error with keyboard" $ do
      let fixture =
             emptyFixture
            { _countRepeat = return 1
            , _isWaitForRepeat = return True
            , _sendMsgKeyboard = \_ -> throwError CannotSendKeyboard
            , _setWaitForRepeat  = \ False -> return ()
            , _writeLogD = \_ -> return ()
            , _writeLogE = \_  -> return ()
            , _nameAdapter = return "Test"
            , _sendMsg = \_ -> return ()
            , _setCountRepeat = \_ -> return ()             
            }
      runApp fixture (handingBotMsg (BotMsg (EmptyMessage "/repeat" 0 1))) `shouldReturn`
         Left CannotSendKeyboard 
  describe "handingBotMsg " $ do
    it "handingBotMsg return error with save count repeat" $ do
      let fixture =
             emptyFixture
            { _countRepeat = return 1
            , _isWaitForRepeat = return False
            , _sendMsgKeyboard = \_ -> return ()
            , _setWaitForRepeat  = \ False -> return ()
            , _writeLogD = \_ -> return ()
            , _writeLogE = \_  -> return ()
            , _nameAdapter = return "Test"
            , _sendMsg = \_ -> return ()
            --   | CannotRepeatFalseNumber
            , _setCountRepeat = \_ -> throwError CannotRepeatCountSet         
            }
      runApp fixture (handingBotMsg (BotMsg (EmptyMessage "test" 0 1))) `shouldReturn`
        Left CannotRepeatCountSet 
    it "handingBotMsg return error with save count repeat" $ do
      let fixture =
             emptyFixture
            { _countRepeat = return 1
            , _isWaitForRepeat = return False
            , _sendMsgKeyboard = \_ -> return ()
            , _setWaitForRepeat  = \ False -> return ()
            , _writeLogD = \_ -> return ()
            , _writeLogE = \_  -> return ()
            , _nameAdapter = return "Test"
            , _sendMsg = \_ -> return ()      
            }  
      runApp fixture (handingBotMsg (BotMsg (EmptyMessage "test" 0 1))) `shouldReturn`
        Left CannotRepeatCountSet 

  describe "handingBotMsgArray " $ do
    it "handingBotMsgArray return Right with empty array" $ do
      let fixture = emptyFixture   
      runApp fixture (handingBotMsgArray []) `shouldReturn`
        Right () 
    it "handingBotMsgArray return error with save count repeat" $ do
      let fixture =
             emptyFixture
            { _countRepeat = return 1
            , _isWaitForRepeat = return False
            , _sendMsgKeyboard = \_ -> return ()
            , _setWaitForRepeat  = \ False -> return ()
            , _writeLogD = \_ -> return ()
            , _writeLogE = \_  -> return ()
            , _nameAdapter = return "Test"
            , _sendMsg = \_ -> return ()      
            }  
      runApp fixture (handingBotMsgArray [BotMsg (EmptyMessage "test" 0 1)]) `shouldReturn`
        Left CannotRepeatCountSet 

    it "handingBotMsgArray return right" $ do
      let fixture =
             emptyFixture
            { _countRepeat = return 1
            , _isWaitForRepeat = return True
            , _sendMsgKeyboard = \_ -> return ()
            , _setWaitForRepeat  = \ False -> return ()
            , _writeLogD = \_ -> return ()
            , _writeLogE = \_  -> return ()
            , _nameAdapter = return "Test"
            , _sendMsg = \_ -> return ()      
            }  
      runApp fixture (handingBotMsgArray  [ BotMsg (EmptyMessage "test" 0 1)
                                          , BotMsg (EmptyMessage "test" 0 1)
                                          ]) `shouldReturn`
        Right () 
    it "handingBotMsgArray return right" $ do
      let fixture =
             emptyFixture
            { _countRepeat = return 1
            , _isWaitForRepeat = return True
            , _sendMsgKeyboard = \_ -> return ()
            , _setWaitForRepeat  = \ False -> return ()
            , _writeLogD = \_ -> return ()
            , _writeLogE = \_  -> return ()
            , _nameAdapter = return "Test"
            , _sendMsg = \_ -> return ()      
            }  
      runApp fixture (handingBotMsgArray  [ BotMsg (EmptyMessage "/repeat" 0 1)
                                          , BotMsg (EmptyMessage "1" 0 1)
                                          ]) `shouldReturn`
        Right () 
    it "handingBotMsgArray return false" $ do
      let fixture =
             emptyFixture
            { _countRepeat = return 1
            , _isWaitForRepeat = return False
            , _sendMsgKeyboard = \_ -> return ()
            , _setWaitForRepeat  = \ False -> return ()
            , _writeLogD = \_ -> return ()
            , _writeLogE = \_  -> return ()
            , _nameAdapter = return "Test"
            , _sendMsg = \_ -> return ()      
            }  
      runApp fixture (handingBotMsgArray  [ BotMsg (EmptyMessage "/repeat" 0 1)
                                          ]) `shouldReturn`
        Left CannotRepeatCountSet 






