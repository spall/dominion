module DominionState where

import Text.Printf
import Data.Either
import Data.Char

class Cost t where
  cost :: t -> Integer

class Value t where
  value :: t -> Integer

type Error = String

data Treasure = Copper | Silver | Gold

instance Cost Treasure where
  cost Copper = 0
  cost Silver = 3
  cost Gold   = 6

instance Value Treasure where
  value Copper = 1
  value Silver = 2
  value Gold   = 3

instance Show Treasure where
  show Copper = "copper"
  show Silver = "silver"
  show Gold   = "gold"

instance Eq Treasure where
  (==) Copper Copper = True
  (==) Silver Silver = True
  (==) Gold   Gold   = True
  (==) _      _      = False

data Victory = Estate | Duchy | Province

instance Cost Victory where
  cost Estate   = 2
  cost Duchy    = 5
  cost Province = 8

instance Value Victory where
  value Estate   = 1
  value Duchy    = 3
  value Province = 6

instance Show Victory where
  show Estate   = "estate"
  show Duchy    = "duchy"
  show Province = "province"

instance Eq Victory where
  (==) Estate   Estate   = True
  (==) Duchy    Duchy    = True
  (==) Province Province = True
  (==) _        _        = False

data Action = Mine | Cellar | Market | Remodel | Smithy | Village | Woodcutter | Workshop | Militia | Moat

instance Cost Action where
  cost Mine       = 5
  cost Cellar     = 2
  cost Market     = 5
  cost Remodel    = 4
  cost Smithy     = 4
  cost Village    = 3
  cost Woodcutter = 3
  cost Workshop   = 3
  cost Militia    = 4
  cost Moat       = 2

instance Show Action where
  show Mine       = "mine"
  show Cellar     = "cellar"
  show Market     = "market"
  show Remodel    = "remodel"
  show Smithy     = "smithy"
  show Village    = "village"
  show Woodcutter = "woodcutter"
  show Workshop   = "workshop"
  show Militia    = "militia"
  show Moat       = "moat"

instance Eq Action where
  (==) Mine Mine             = True
  (==) Cellar Cellar         = True
  (==) Market Market         = True
  (==) Remodel Remodel       = True
  (==) Smithy Smithy         = True
  (==) Village Village       = True
  (==) Woodcutter Woodcutter = True
  (==) Workshop Workshop     = True
  (==) Militia Militia       = True
  (==) Moat Moat             = True
  (==) _ _                   = False

data Card = T Treasure | V Victory | A Action

instance Cost Card where
  cost (T treasure) = cost treasure
  cost (V victory)  = cost victory
  cost (A action)   = cost action

instance Show Card where
  show (T treasure) = show treasure
  show (V victory)  = show victory
  show (A action)   = show action
  showList cl = (++) (foldl (\prev card -> prev++" "++(show card)) "" cl)

instance Eq Card where
  (==) (T c1) (T c2) = c1 == c2
  (==) (V c1) (V c2) = c1 == c2
  (==) (A c1) (A c2) = c1 == c2
  (==) _      _      = False
                                
-- game state

data Player = Player { name :: String }

instance Show Player where
  show Player{name=n} = n  
  showList pl = (++) (foldl (\prev player -> prev++" "++(show player)) "" pl)

data GameState = GameState { players :: [Player]
                           , supply :: [Card]
                           , trash :: [Card]
                           , actions :: Integer
                           , buys :: Integer
                           , coins :: Integer
                           , deck :: [Card]
                           , hand :: [Card]
                           , plays :: [Card]
                           , discards :: [Card]
                           }

instance Show GameState where
  show gs = "( "++ (showElement "players" $ players gs) ++ "\n"
            ++ (showElement "supply" $ supply gs) ++ "\n"
            ++ (showElement "trash" $ trash gs) ++ "\n"
            ++ (showElement "actions" $ actions gs) ++ "\n"
            ++ (showElement "buys" $ buys gs) ++ "\n"
            ++ (showElement "coins" $ coins gs) ++ "\n"
            ++ (showElement "deck" $ deck gs) ++ "\n"
            ++ (showElement "hand" $ hand gs) ++ "\n"
            ++ (showElement "plays" $ plays gs) ++ "\n"
            ++ (showElement "discards" $ discards gs) ++ " )"

showElement :: (Show a) => String -> a -> String
showElement label elem = "( "++label++" "++(show elem)++" )"


-- other --
data Play = Clean { maybeCard::(Maybe Card) }
          | Buy { card::Card }
          | Add { treasure::Treasure }
          | Act { action::Action
                , info::[Card] }

instance Show Play where
  show Clean{maybeCard=Nothing}  = "(" ++ "clean" ++ ")"
  show Clean{maybeCard=(Just c)} = "(" ++ "clean " ++ (show c) ++ ")"
  show Buy{card=c}     = "(" ++ "buy " ++ (show c) ++ ")"
  show Add{treasure=t} = "(" ++ "add " ++ (show t) ++ ")"
  show Act{action=a
          , info=i }   = "(" ++ "act " ++ (show a) ++ " " ++ (show i) ++ ")"

data Defense = CardDefense { defenseAction::Action }
             | Discard { cards::[Card] }

instance Show Defense where
  show CardDefense{defenseAction=da} = "(" ++ (show da) ++ ")"
  show Discard{cards=cs}             = "(" ++ "discard" ++ " "  ++ (show cs) ++ ")"


data Notification = Attacked { play::Play
                             , player::Player
                             , state::GameState }
                  | Defended { player::Player
                             , defense::Defense }
                  | Move { state::GameState }
                  | Moved { player::Player
                          , play::Play }

