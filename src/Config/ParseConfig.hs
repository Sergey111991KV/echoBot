module Config.ParseConfig where

import ClassyPrelude
    ( ($), Monad(return), String, Text, Alternative((<|>)) ) 
import qualified Text.Parsec as Parsec

type ConfigPair = (String, String)


toPairs :: Parsec.Parsec Text () ConfigPair
toPairs = do
  key <- Parsec.many1 (Parsec.letter <|> Parsec.digit <|> Parsec.char ':')
  Parsec.spaces
  value <-
    Parsec.many1 (Parsec.letter <|> Parsec.digit <|> Parsec.char ':') <|>
    helpText
  return (key, value)

myParserPair :: Parsec.Parsec Text () [ConfigPair]
myParserPair = Parsec.many1 $ do
  pair <- toPairs 
  Parsec.eof <|> mySeparator
  return pair


myParser :: Parsec.Parsec Text () ([ConfigPair],String)
myParser = do
  pair <- myParserPair
  othetText <- Parsec.many1 (Parsec.letter <|> Parsec.digit <|> Parsec.space) <|> defText
  return (pair,othetText)

defText :: Parsec.Parsec Text () String 
defText = return []

mySeparator :: Parsec.Parsec Text () ()
mySeparator = do
  _ <- Parsec.char '\n' 
  return ()

helpText :: Parsec.Parsec Text () String
helpText = do
  _ <- Parsec.char '"'
  value <- Parsec.many1 (Parsec.letter <|> Parsec.digit <|> Parsec.space)
  _ <- Parsec.char '"'
  return value
