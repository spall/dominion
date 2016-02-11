module DominionState where

import Text.Printf
import Data.Either
import Data.Char

type Error = String

data Card = Copper   { value::Int
                     , cost::Int
                     }
          | Silver   { value::Int
                     , cost::Int
                     }
          | Gold     { value::Int
                     , cost::Int
                     }
          | Estate   { points::Int
                     , cost::Int
                     }
          | Duchy    { points::Int
                     , cost::Int
                     }
          | Province { points::Int
                     , cost::Int
                     }
          | Mine     { act::(GameState -> GameState)
                     }

instance Show Card where
  show Copper{}   = "copper"
  show Silver{}   = "silver"
  show Gold{}     = "gold"
  show Estate{}   = "estate"
  show Duchy{}    = "duchy"
  show Province{} = "province"
  show Mine{}     = "mine"

  showList cl = (++) (foldl (\prev card -> prev++" "++(show card)) "" cl)
                                
copper :: Card
copper = Copper 1 0

silver :: Card
silver = Silver 2 3

gold :: Card
gold = Gold 3 6

estate :: Card
estate = Estate 1 2

duchy :: Card
duchy = Duchy 3 5

province :: Card
province = Province 6 8

mine :: Card
mine = Mine (\x -> x)

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
