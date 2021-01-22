module Bot.Message where

import ClassyPrelude

data BotMsg =
  forall a. BotCompatibleMsg a =>
            BotMsg a 

class BotCompatibleMsg a where
  textMsg :: a -> Text
  chatId :: a -> Int
  idMsg :: a -> Int


findMaxUpd ::  [BotMsg] -> Int
findMaxUpd arr = maximum' $ getIdsMsg arr



getIdsMsg :: [BotMsg] ->  [Int]
getIdsMsg [] =  []
getIdsMsg (BotMsg x:xs) = [idMsg x] <> getIdsMsg xs

findLastMsgs ::  Int -> [BotMsg] -> [ BotMsg]
findLastMsgs _ [] =  []
findLastMsgs lastId arr = mapMaybe' u
    where
      u = fmap (findLastMsg lastId)  arr


findLastMsg :: Int -> BotMsg ->  Maybe BotMsg
findLastMsg lastId (BotMsg msg) = if lastId < idMsg msg
                            then  
                              Just  (BotMsg msg) else Nothing



-- у меня не получилось использовать библиотечные функции - поэтому я написал их сам

mapMaybe' :: [Maybe a] -> [a]
mapMaybe' [] = []
mapMaybe' (Nothing:xs) =  mapMaybe' xs
mapMaybe' (Just a: xs) = [a] <> mapMaybe' xs

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










--   isEmpty :: a -> Bool

-- data EmptyMsg = EmptyMsg {
--   emptyTextMsg ::  Text
--   , emptyChatId ::  Int
--   , emptyIdMsg ::  Int
-- }

-- instance BotCompatibleMsg EmptyMsg where
--     textMsg = emptyTextMsg
--     chatId = emptyChatId
--     idMsg = emptyIdMsg
--     isEmpty _ = True

-- emptyMsg :: EmptyMsg
-- emptyMsg = EmptyMsg "" 0 0