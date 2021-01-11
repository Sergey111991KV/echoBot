module Bot.EchoBot where

import ClassyPrelude
    ( ($),
      Monad(return, (>>)),
      Num((-)),
      Ord((>), (<=)),
      Semigroup((<>)),
      Bool(..),
      Integer,
      Maybe(..),
      Either(..),
      (.),
      MonadIO(..),
      (&&),
      Text,
      readMay )

import Control.Monad.Except
    ( MonadError(..) )
import Control.Concurrent ( threadDelay )

import Bot.Message (BotCompatibleMessage(textMsg), BotMsg(..))
import Log.ImportLog (Log(writeLogD, writeLogE))
import Bot.Bot ( Bot(..) )
import Bot.Error
    ( Error(NotAnswer, CannotRepeatFalseNumber, NotNewMsg,
            CantConvertFromData, CantConvertFromArray, CannotRepeatCountSet),
      errorText )



class (Bot m ,MonadError Error m )=>
      EchoBot m
  where
  msgHelp :: m Text
  countRepeat :: m Integer
  isWaitForRepeat :: m Bool
  setWaitForRepeat :: Bool -> m ()
  setCountRepeat :: Integer -> m ()
  sendMsgKeyboard :: BotMsg -> m ()
  nameAdapter :: m Text

sendMsgEcho :: (MonadIO m, EchoBot m) => BotMsg -> m ()
sendMsgEcho msg = do
  repCount <- countRepeat
  msgCountRepeat repCount msg

msgCountRepeat :: EchoBot m => Integer -> BotMsg -> m  ()
msgCountRepeat count meesBot = do
  case count of
    0 -> do
      return  ()
    _ -> do
      sendMsg meesBot
      msgCountRepeat (count - 1) meesBot

tryGetCountRepeat :: (MonadIO m, EchoBot m) => BotMsg -> m ()
tryGetCountRepeat (BotMsg msg) = do
  let resultKeyboardAnswer = (readMay (textMsg msg) :: Maybe Integer)
  case resultKeyboardAnswer of
    Nothing ->
      throwError CannotRepeatCountSet
    Just newCount -> do
      if newCount <= 5 && newCount > 0
        then do
          setCountRepeat newCount
        else
          throwError CannotRepeatFalseNumber

handingBotMsg :: (MonadIO m, EchoBot m) => BotMsg -> m  ()
handingBotMsg (BotMsg msg) = do
  let txtMsg = textMsg msg
  isWait <- isWaitForRepeat
  if isWait
    then do
      case txtMsg of
        "/help" -> do
          helpMsg <- msgHelp
          sendMsgHelp helpMsg (BotMsg msg)
        "/repeat" -> do
          sendMsgKeyboard (BotMsg msg)
          setWaitForRepeat False
        _ -> do
          sendMsgEcho (BotMsg msg)
    else do
      tryGetCountRepeat (BotMsg msg)
      setWaitForRepeat True

-- зачем возвращать m Either, когда EchoBot m => MonadError Error m?
-- т.е. в m уже есть эффект ошибки
processEchoBot :: (MonadIO m, EchoBot m) => m (Either Error ())
processEchoBot = do
  msg <- getMsgLast
  nameAd <- nameAdapter
  writeLogD ("getMsgLast " <> nameAd)
  handingBotMsg msg
  -- всегда возвращает Right
  return $ Right ()

-- тоже самое по смыслу:
{-
processEchoBot :: (MonadIO m, EchoBot m) => m ()
processEchoBot = do
  msg <- getMsgLast
  nameAd <- nameAdapter
  writeLogD ("getMsgLast " <> nameAd)
  handingBotMsg msg
  return ()
-}

finalEchoBot :: (MonadIO m, EchoBot m) => m  ()
finalEchoBot = do
  nameA <- nameAdapter
  processBot <-  processEchoBot `catchError` ( return . Left )
  -- тогда здесь будет что то типа:
  -- (processEchoBot >> finalEchoBot) `catchError` \err -> (все из Left err -> do)
  case processBot of
    Right () -> do
      finalEchoBot
    Left err -> do
      writeLogE $ errorText err <> nameA
      case err of
        NotNewMsg ->
          -- телеге нужна эта задержка, вк нети
           liftIO (threadDelay 1000000) >>
          finalEchoBot
        CantConvertFromData ->
          -- в случае всех остальных ошибок лучше подождать несколько секунд, может
          -- это проблема с сервером или сетью
          finalEchoBot
        CantConvertFromArray ->
          finalEchoBot
        CannotRepeatCountSet ->
          finalEchoBot
        NotAnswer ->
          -- не прекратит ли это выполнение всего потока? почему остальные ошибки
          -- мы либо игнорируем, либо повторяем пока не получится, но только
          -- в этом случае выкидываем ексепшн?
          throwError NotAnswer
        _ -> return ()



