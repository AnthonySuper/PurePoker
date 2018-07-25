{-# LANGUAGE OverloadedStrings #-}
module Game.PokerPlayerSpec where 

    import Game.Poker 

    import Game.PokerPlayer 

    import Test.Hspec

    import Control.Lens


    import qualified Data.List.PointedList.Circular as PL


    spec = do
        let players = map (createPlayer 10000) ["bob", "joe", "jim"] 
        let game = createPokerGame players 100 50 0 
        describe "dealing cards" $ do
            let action = dealAllIf playerHoleCards (const True)
            let ran = runIdentity $ runGameState action game 
            let holeCards = pokerGamePlayers . traverse . playerHoleCards
            let justState = snd <$> ran
            it "can deal a hole card to everybody" $
                (map length . toListOf holeCards) <$> justState `shouldBe` pure [1, 1, 1]
        