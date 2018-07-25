module Game.PokerSpec where 

    import Game.Poker 

    import Test.Hspec

    spec = 
        describe "Hand evaluation" $ do
            let shouldEvalSatisfy a b = toRanked (PokerHand a) `shouldSatisfy` b
            let shouldNotEvalSatisfy a b = toRanked (PokerHand a) `shouldNotSatisfy` b
            describe "flushes" $ do
                it "recognizes a real flush" $ 
                    [ Card Two Diamonds
                    , Card Three Diamonds
                    , Card Four Diamonds 
                    , Card Six Diamonds 
                    , Card Seven Diamonds ] `shouldEvalSatisfy` isFlush
                it "doesn't recognize a straight flush" $
                    [ Card Two Diamonds
                    , Card Three Diamonds
                    , Card Four Diamonds 
                    , Card Five Diamonds
                    , Card Six Diamonds ] `shouldNotEvalSatisfy` isFlush
            describe "high cards" $ 
                it "recognizes a high card only hand" $ 
                    [ Card Two Diamonds
                    , Card Three Diamonds 
                    , Card Four Diamonds 
                    , Card Six Clubs
                    , Card Seven Clubs ] `shouldEvalSatisfy` isHighCard
            describe "pairs" $
                it "recognizes a pair" $ 
                    [ Card Two Clubs
                    , Card Two Diamonds 
                    , Card Three Clubs
                    , Card Four Clubs
                    , Card Five Clubs ] `shouldEvalSatisfy` isPair 
            describe "two pairs" $ do
                let hand' = [ Card Two Clubs
                            , Card Two Diamonds 
                            , Card Three Clubs
                            , Card Three Diamonds 
                            , Card Four Clubs]
                let hand = toRanked $ PokerHand hand'
                it "recognizes two pairs" $ 
                    hand `shouldSatisfy` isTwoPair
            
                    
                


    