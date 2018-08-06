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
        let toGameRes act g s = runIdentity $ (runGameStateState act g s)
        describe "dealing cards" $ do
            let action = dealAllIf playerHoleCards (const True)
            let ran = runIdentity $ runGameState action game 10
            let holeCards = pokerGamePlayers . traverse . playerHoleCards
            let justState = snd <$> ran
            it "can deal a hole card to everybody" $
                map length . toListOf holeCards <$> justState `shouldBe` pure [1, 1, 1]
        describe "shuffling" $ do
            let f = toGameRes shuffleDeck game 
            it "is the same with the same seed" $ 
                f 10 `shouldBe` f 10
            it "is different with different seeds" $ 
                f 10 `shouldNotBe` f 12