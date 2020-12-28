module Config.ParseConfig where

<<<<<<< HEAD
import ClassyPrelude (Alternative((<|>)), Monad(return), String, Text)
=======
import ClassyPrelude
    ( ($), Monad(return), String, Text, Alternative((<|>)) ) 
>>>>>>> master2
import qualified Text.Parsec as Parsec

type ConfigPair = (String, String)


toPair :: Parsec.Parsec Text () ConfigPair
toPair = do
  key <- Parsec.many1 (Parsec.letter <|> Parsec.digit <|> Parsec.char ':')
  Parsec.spaces
  value <-
    Parsec.many1 (Parsec.letter <|> Parsec.digit <|> Parsec.char ':') <|>
    helpText
  return (key, value)

<<<<<<< HEAD
myParser :: Parsec.Parsec Text () [ConfigPair]
myParser = Parsec.sepBy toPair mySeparator

mySeparator :: Parsec.Parsec Text () ()
mySeparator = do
  _ <- Parsec.char '\n'
=======
toPairArray :: Parsec.Parsec Text () [ConfigPair]
toPairArray = Parsec.many1 $ do
  pair <- toPair 
  Parsec.eof <|> mySeparator
  return pair


defText :: Parsec.Parsec Text () String 
defText = return []


mySeparator :: Parsec.Parsec Text () ()
mySeparator = do
  _ <- Parsec.char '\n' 
>>>>>>> master2
  return ()

helpText :: Parsec.Parsec Text () String
helpText = do
  _ <- Parsec.char '"'
  value <- Parsec.many1 (Parsec.letter <|> Parsec.digit <|> Parsec.space)
  _ <- Parsec.char '"'
  return value
<<<<<<< HEAD
=======

myParser :: Parsec.Parsec Text () ([ConfigPair],String)
myParser = do
  pair <- toPairArray 
  othetText <- Parsec.many1 (Parsec.letter <|> Parsec.digit <|> Parsec.space) <|> defText
  return (pair,othetText)
>>>>>>> master2
