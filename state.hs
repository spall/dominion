module DominionState where

import Text.Printf
import Data.Either
import Data.Char

class Cost t where
  cost :: t -> Int

class Value t where
  value :: t -> Int

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

data Action = Mine

instance Cost Action where
  cost Mine = 5

instance Show Action where
  show Mine = "mine"

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


-- Action cards

-- trash a treasure card for a new treasure card of no more than +1 value.
-- new card goes in hand.

{-
mine :: Card -- need to update state. todo
mine = Mine (\gs@GameState{hand=cards} -> case mineLeastValuable cards of
                                       (old,new) -> (gs, mineMessage old new))

mineLeastValuable :: [Card] -> (Card, Card) --(old_card, new_card)
mineLeastValuable cards = let lsc = foldl (\prev card ->
                                           case (prev, card) of
                                           (Copper,_) -> prev
                                           (_,Copper) -> card
                                           (Silver,_) -> prev
                                           (_,Silver) -> card
                                           (Gold,_)   -> prev
                                           (_,_)        -> card) (head cards) (tail cards) in
                          (lsc, mineTreasure lsc)
                          
mineTreasure :: Card -> Card
mineTreasure Copper = Silver
mineTreasure Silver = Gold
mineTreasure Gold = Gold

mineMessage :: Card -> Card -> String
mineMessage old_card new_card = "("++ "act" ++ " " ++ "mine" ++ " " ++ (show old_card) ++ " " ++ (show new_card) ++ ")"

-}
