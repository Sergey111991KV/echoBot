module Bot.MessageSpec where

import ClassyPrelude ( ($), Int, Text, length )
import Test.Hspec ( describe, it, shouldBe, Spec )
import Bot.Message
    ( BotMsg(BotMsg), BotCompatibleMsg(..), findLastMsgs, findMaxUpd )

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
    describe "findMaxUpd" $ do
        it "should find max Id from message" $ do
            findMaxUpd      [   BotMsg (EmptyMessage "" 0 1)
                            ,   BotMsg (EmptyMessage "" 0 3)
                            ,   BotMsg (EmptyMessage "" 0 10)
                            ,   BotMsg (EmptyMessage "" 0 6)
                            ] `shouldBe` 10
        it "should return 0 from empty array" $ do
            findMaxUpd    [ ] `shouldBe` 0
    describe "findLastMsgs" $ do
        it "should find max Id from message" $ do
           length (findLastMsgs    0   [   BotMsg (EmptyMessage "" 0 1)
                                    ,   BotMsg (EmptyMessage "" 0 3)
                                    ,   BotMsg (EmptyMessage "" 0 10)
                                    ,   BotMsg (EmptyMessage "" 0 6)
                                ] ) `shouldBe` 4
        it "should return 0 from empty array" $ do
            length (findLastMsgs    10     [   BotMsg (EmptyMessage "" 0 1)
                            ,   BotMsg (EmptyMessage "" 0 3)
                            ,   BotMsg (EmptyMessage "" 0 10)
                            ,   BotMsg (EmptyMessage "" 0 6)
                            ] )`shouldBe` 0