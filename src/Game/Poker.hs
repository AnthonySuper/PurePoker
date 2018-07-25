{-# LANGUAGE DeriveGeneric #-}
module Game.Poker where

    import Data.Maybe
    import Data.List
    import Control.Applicative
    import GHC.Generics
    import Control.Arrow
    import Control.Monad
    import Data.Ord
    import Data.Aeson

    data Suit
        = Hearts | Diamonds | Spades | Clubs
        deriving (Show, Eq, Ord, Bounded, Enum, Generic)

    instance ToJSON Suit
    instance FromJSON Suit 

    data Rank
        = Two
        | Three
        | Four
        | Five 
        | Six 
        | Seven 
        | Eight 
        | Nine 
        | Ten
        | Jack 
        | Queen 
        | King
        | Ace 
        deriving (Show, Eq, Ord, Bounded, Enum, Generic)

    instance ToJSON Rank 
    instance FromJSON Rank 

    data Card = Card { rank :: Rank, suit :: Suit }
        deriving (Show, Eq, Ord, Bounded, Generic)

    instance ToJSON Card
    instance FromJSON Card

    allSuits = [(minBound :: Suit) ..]

    allRanks = [(minBound :: Rank) ..]

    fullDeck :: [Card]
    fullDeck = Card <$> allRanks <*> allSuits 

    -- |A PokerHand represents an evaluated, orderable poker hand
    data RankedPokerHand
        -- | A high card, with kickers
        = HighCard [Rank]
        -- | A pair, with three kickers
        | Pair Rank Rank Rank Rank
        -- | Two pair, with one kicker
        | TwoPair Rank Rank Rank 
        -- | Three of a kind, with two kickers
        | ThreeKind Rank Rank Rank
        -- | A straight, which only needs to hold the highest card in it
        | Straight Rank
        -- | A flush, with kickers
        | Flush [Rank]
        -- | A full house, with the two ranks involved 
        | FullHouse Rank Rank
        -- | A four of a kind, with kicker
        | FourKind Rank Rank
        | StraightFlush Rank
        deriving (Show, Eq, Ord, Generic)

    instance ToJSON RankedPokerHand
    instance FromJSON RankedPokerHand

    newtype PokerHand = PokerHand [Card]
        deriving (Show, Eq, Ord, Generic)

    instance ToJSON PokerHand
    instance FromJSON PokerHand
    
    isHighCard (HighCard _) = True 
    isHighCard _ = False 

    isPair Pair{} = True 
    isPair _  = False

    isTwoPair TwoPair{} = True
    isTwoPair _ = False

    isThreeKind ThreeKind{} = True 
    isThreeKind _ = False 

    isStraight (Straight _) = True 
    isStraight _ = False 
    
    isFlush (Flush _) = True 
    isFlush _ = False 
    
    isFullHouse FullHouse{} = True
    isFullHouse _ = False 

    isFourKind FourKind{} = True 
    isFourKind _ = False 

    isStraightFlush StraightFlush{} = True 
    isStraightFlush _ = False 

    isRoyalFlush (StraightFlush Ace) = True
    isRoyalFlush _ = False


    -- | Convert a PokerHand to a ranked hand
    -- | Thanks to https://www.reddit.com/r/haskell/comments/3r8x5m/best_way_to_model_a_deck_of_cards/cwo5lks/
    -- | for this bit of the code
    toRanked (PokerHand cards)
        | Just hiRank <- flush >> straight = StraightFlush hiRank
        | [(4, quadRank), (1, offRank)] <- groups = FourKind quadRank offRank
        | [(3, tripRank), (2, pairRank)] <- groups = FullHouse tripRank pairRank
        | Just _ <- flush = Flush ranks
        | Just hiRank <- straight = Straight hiRank 
        | [(3, tripRank), (1, nextRank), (1, lastRank)] <- groups = ThreeKind tripRank nextRank lastRank
        | [(2, hiRank), (2, loRank), (1, offRank)] <- groups = TwoPair hiRank loRank offRank 
        | [(2, pairRank), (1, highKick), (1, midKick), (1, lastKick)] <- groups = Pair pairRank highKick midKick lastKick 
        | otherwise = HighCard ranks 
        where
            ranks = sortOn Down $ map rank cards
            groups = getGroups ranks
            straight = getStraight ranks 
            flush = getFlush $ map suit cards 

    getStraight :: [Rank] -> Maybe Rank 
    getStraight ranks = guard monotonicDecreasingRanks >> (normal <|> wheel)
        where
            allAdjacent p xs = and $ zipWith p xs (tail xs)
            monotonicDecreasingRanks = allAdjacent (>) ranks
            normal | allAdjacent (\a b -> a == succ b) ranks = Just (head ranks)
                | otherwise = Nothing 
            wheel  | [Ace, Five, Four, Three, Two] == ranks = Just Five 
                | otherwise = Nothing 
    
    getFlush :: [Suit] -> Maybe Suit 
    getFlush suits = case nub suits of 
        [s] -> Just s 
        _ -> Nothing 

    getGroups :: [Rank] -> [(Int, Rank)]
    getGroups = sortOn Data.Ord.Down . map (length &&& head) . group

    combinations :: Int -> [a] -> [[a]]
    combinations 0 _  = [ [] ]
    combinations n xs = [ y:ys | y:xs' <- tails xs
                            , ys <- combinations (n-1) xs']
                           
    toHandPair :: [Card] -> (RankedPokerHand, PokerHand)
    toHandPair cards = (toRanked p, p) 
        where p = PokerHand cards

    bestHand :: [Card] -> (RankedPokerHand, PokerHand)
    bestHand = head . sortOn (Down . fst) . (toHandPair <$>) . combinations 5