{-# LANGUAGE DeriveGeneric, 
             TemplateHaskell, 
             RankNTypes, 
             FlexibleContexts, 
             OverloadedStrings, 
             LambdaCase,
             NoMonomorphismRestriction,
             GeneralizedNewtypeDeriving  #-}
module Game.PokerPlayer where

    import Game.Poker 
    import GHC.Generics
    import qualified Data.Text as T
    import qualified Data.List.PointedList.Circular as PL 
    import Data.Aeson
    import Control.Lens
    import Data.List.Lens
    import Control.Monad
    import Data.List (sort)
    import System.Random (StdGen, mkStdGen)
    import System.Random.Shuffle
    import Control.Monad.Trans.Random.Strict 
    import Control.Monad.Random.Class
    import Control.Monad.State.Strict
    import Control.Monad.Except 
    import Data.Foldable (toList)
    import Data.Maybe (fromJust)
    type PlayerId = T.Text

    data Player
        = Player { _playerId :: PlayerId 
                 , _playerStackSize :: Integer 
                 , _playerCurrentBet :: Integer
                 , _playerHoleCards :: [Card]
                 , _playerVisibleCards :: [Card]
                 , _playerFolded :: Bool
                 }
        deriving (Eq, Show, Generic)

    makeLenses ''Player
    instance ToJSON Player
    instance FromJSON Player

    data PokerGame 
        = PokerGame { _pokerGamePlayers :: PL.PointedList Player
                    , _pokerGameCommunityCards :: [Card]
                    , _pokerGameDeck :: [Card]
                    , _pokerGameBigBlind :: Integer
                    , _pokerGameSmallBlind :: Integer
                    , _pokerGameAnte :: Integer
                    }
        deriving (Eq, Show, Generic)

    instance ToJSON a => ToJSON (PL.PointedList a) where 
        toJSON l =
            object ["elements" Data.Aeson..= toList l, "current" Data.Aeson..= PL.index l]

    instance FromJSON a => FromJSON (PL.PointedList a) where
        parseJSON (Object o') = obj o' >>= \case  
            Just p -> pure p
            Nothing -> fail "No empty lists!"
            where 
                obj o = makeList <$> o .: "elements" <*> o .: "current"
                makeList e f = PL.moveN f <$> PL.fromList e

    
    makeLenses ''PokerGame
    instance ToJSON PokerGame
    instance FromJSON PokerGame

    data PokerError 
        = PlayerNotFound T.Text
        | EmptyDeck
        | BadDeal
        | PlayerExists PlayerId
        | ImpossibleState
        | AllFold
        deriving (Eq, Show, Generic)

    instance ToJSON PokerError 
    instance FromJSON PokerError 

    currentPlayer = pokerGamePlayers . PL.focus

    newtype GameState m a = GameState {
        runGameStatea :: StateT PokerGame (RandT StdGen (ExceptT PokerError m)) a
    } deriving ( Functor
               , Applicative 
               , Monad 
               , MonadIO
               , MonadState PokerGame 
               , MonadError PokerError
               , MonadRandom
               , MonadFix)


    runGameState :: (Monad m) => GameState m a -> PokerGame -> Int -> m (Either PokerError (a, PokerGame))
    runGameState g initState seed = runExceptT $ evalRandT stated (mkStdGen seed)
        where stated = runStateT (runGameStatea g) initState

    runGameStateState g i s = (fmap . fmap) snd (runGameState g i s)

    createPlayer :: Integer -> PlayerId -> Player
    createPlayer ss id
        = Player { _playerId = id
                 , _playerStackSize = ss 
                 , _playerCurrentBet = 0
                 , _playerHoleCards = []
                 , _playerVisibleCards = []
                 , _playerFolded = False }

    createPokerGame :: [Player] -> Integer -> Integer -> Integer -> PokerGame
    createPokerGame players bb sb ant = 
        PokerGame { _pokerGamePlayers = (fromJust . PL.fromList) players 
                  , _pokerGameCommunityCards = []
                  , _pokerGameDeck = fullDeck 
                  , _pokerGameBigBlind = bb
                  , _pokerGameSmallBlind = sb 
                  , _pokerGameAnte = ant
                  }

    orError :: (MonadError e m) => e -> Maybe a -> m a 
    orError e (Just x) = pure x
    orError e Nothing = throwError e 

    drawCard p = orError EmptyDeck $ do 
        let deck = view pokerGameDeck p
        (c, d) <- uncons deck 
        return (c, over pokerGameDeck (const d) p)

    dealCard selector game = do
        (c, d) <- drawCard game 
        orError BadDeal $ failover selector (|> c) game

    joinGame :: MonadError PokerError g => Player -> PokerGame -> g PokerGame
    joinGame p g = if p ^. playerId `elem` ((^. playerId) <$> g ^. pokerGamePlayers) 
        then throwError $ PlayerExists (p ^. playerId)
        else pure $ over pokerGamePlayers (PL.insert p) g

    shuffleDeck = do
        s <- get
        n <- shuffleM $ s ^. pokerGameDeck 
        pokerGameDeck Control.Lens..= n

    dealCommunityCard = dealCard pokerGameCommunityCards

    dealCurrentPlayer sel = dealCard $ currentPlayer . sel

    dealCurrentHole = dealCurrentPlayer playerHoleCards 

    dealCurrentVisible = dealCurrentPlayer playerVisibleCards

    shiftCurrent = pokerGamePlayers %~ PL.next

    allFolded g = length foldList < 2 
        where
            foldList = filter (^. playerFolded) (toList $ g ^. pokerGamePlayers)

    
    dealAllIf sel filt = do 
        g <- get 
        replicateM_ (PL.length $ g ^. pokerGamePlayers) (dealIf >> modify shiftCurrent)
        where
            dealIf = do
                g <- get 
                if filt $ g ^. pokerGamePlayers . PL.focus then do
                    ng <- dealCurrentPlayer sel g 
                    put ng 
                else pure ()
    
    stripPrivateExcept :: PlayerId -> PokerGame -> PokerGame
    stripPrivateExcept i = pokerGamePlayers %~ stripMap
        where
            stripMap = fmap stripIfNot
            stripIfNot p = if p ^. playerId == i then p else stripPrivate p
            stripPrivate = playerHoleCards .~ []