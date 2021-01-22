module Log.Log where

import ClassyPrelude
  ( Bool
  , FilePath
  , IO
  , LazySequence(toStrict)
  , Ord((>=))
  , Semigroup((<>))
  , Text
  , UTCTime
  , ($)
  , (.)
  , putStrLn
  , unpack
  , when
  )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Time (formatISODateTime)
import System.IO (appendFile)

import Log.LogEntity (LogConfig(LogConfig), LogWrite)

toStrict1 :: BL.ByteString -> B.ByteString
toStrict1 = B.concat . BL.toChunks

writeInLogFile :: FilePath -> Bool -> Text -> IO ()
writeInLogFile lF bl txtInLog = do
  when bl $ appendFile lF (ClassyPrelude.unpack txtInLog)

writeInTerminal :: Bool -> Text -> IO ()
writeInTerminal bl txtInLog = do
  when bl $ ClassyPrelude.putStrLn txtInLog

writFileHandler ::
     UTCTime -> FilePath -> LogWrite -> LogWrite -> Bool -> Text -> IO ()
writFileHandler dat lF logConf logToCompare bl txtInLog = do
  writeInLogFile lF (logConf >= logToCompare) (txtInLog <> " " <> d)
  writeInTerminal bl txtInLog
  where
    d = toStrict $ formatISODateTime dat

writeLogHandler :: UTCTime -> LogConfig -> LogWrite -> Text -> IO ()
writeLogHandler dat (LogConfig lf logLev logBool) loging =
  writFileHandler dat lf logLev loging logBool
