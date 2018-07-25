{-# LANGUAGE DeriveGeneric #-}
module Game.TexasHoldEm (TexasHoldEmEvent)
    where

    import Game.PokerPlayer (PlayerId, PokerGame)
    import GHC.Generics
    import Data.Aeson

    data TexasHoldEmEvent
        = Check 
        | Call 
        | Raise Integer 
        | Fold
        deriving (Eq, Show, Generic)

    instance ToJSON TexasHoldEmEvent
    instance FromJSON TexasHoldEmEvent