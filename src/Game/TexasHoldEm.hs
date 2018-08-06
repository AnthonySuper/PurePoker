{-# LANGUAGE DeriveGeneric
           , NoMonomorphismRestriction #-}
module Game.TexasHoldEm (TexasHoldEmEvent)
    where

    import Game.Poker
    import Game.PokerPlayer
    import GHC.Generics
    import Data.Aeson
    import Control.Monad.Fix 
    import Control.Monad.State.Class
    import Control.Monad.Error.Class
    import Control.Lens
    import Control.Monad

    data TexasHoldEmEvent
        = Check 
        | Call 
        | Raise Integer 
        | Fold
        deriving (Eq, Show, Generic)

    data TexasHoldEmResponse
        = NextPlayer 
        | EndRound
        | EndHand
        deriving (Eq, Show, Generic)

    instance ToJSON TexasHoldEmEvent
    instance FromJSON TexasHoldEmEvent

    monadicModify x = do 
        state <- get 
        state' <- x state
        put state'

        
    holdEmGame :: GameState IO ()
    holdEmGame = do
        dealHoleCards
        runBetSet
        replicateM_ 3 (monadicModify dealCommunityCard)
        runBetSet
        monadicModify dealCommunityCard
        runBetSet
        monadicModify dealCommunityCard
        runBetSet
        evaluateWinner
        resetState 

    dealHoleCards = undefined
    
    runBetSet :: GameState IO ()
    runBetSet = do
        s <- get 
        when (allFolded s) $ throwError AllFold
        if betsEqual g then
            return ()
        else do 
            getCurrentBet
            



    evaluateWinner = undefined
    resetState = undefined
