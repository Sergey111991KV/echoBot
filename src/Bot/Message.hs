{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Bot.Message where

import ClassyPrelude


data BotMsg =
  forall a. BotCompatibleMsg a =>
            BotMsg a 
            
-- instance Show BotMsg where
--   show (BotMsg a) = show $ textMsg a


class BotCompatibleMsg a where
  textMsg :: a -> Text
  chatId :: a -> Int
  idMsg :: a -> Int


findMaxUpd ::  [BotMsg] -> Int
findMaxUpd arr = maximum'  $ getIdsMsg arr



getIdsMsg :: [BotMsg] ->  [Int]
getIdsMsg [] =  []
getIdsMsg (BotMsg x:xs) = [idMsg x] <> getIdsMsg xs

findLastMsgs ::  Int -> [BotMsg] -> [ BotMsg]
findLastMsgs _ [] =  []
findLastMsgs lastId arr = catMaybes u
    where
      u = fmap (findLastMsg lastId)  arr


findLastMsg :: Int -> BotMsg ->  Maybe BotMsg
findLastMsg lastId (BotMsg msg) = if lastId < idMsg msg
                            then  
                              Just  (BotMsg msg) else Nothing


maximum' :: [Int] -> Int
maximum' [] = 0
maximum' [x] = x
maximum' (x:xs) = if x > head' xs then maximum' (x: tail' xs) else maximum' xs

tail' ::  [Int] -> [Int]
tail' [] = []
tail' [x] = [x]
tail' (_:xs) = xs

head' ::[Int] -> Int
head' [] = 0
head' [x] = x
head' (x:_) = x