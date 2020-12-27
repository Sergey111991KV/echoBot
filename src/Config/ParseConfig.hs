module Config.ParseConfig where

import ClassyPrelude (Alternative((<|>)), Monad(return), String, Text)
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

myParser :: Parsec.Parsec Text () [ConfigPair]
myParser = (Parsec.sepBy toPairs mySeparator)


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
