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

data GameState = GameState { players :: [String]
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
  show GameState{players   = p  -- do show for all of the lists and cons onto the front and end parens
                , supply   = s  -- and the word
                , trash    = t
                , actions  = a
                , buys     = b
                , coins    = c
                , deck     = d
                , hand     = h
                , plays    = p2
                , discards = d2} = "( "++ (showElement "players" p) ++ "\n"
                                       ++ (showElement "supply" s) ++ "\n"
                                       ++ (showElement "trash" t) ++ "\n"
                                       ++ (showElement "actions" a) ++ "\n"
                                       ++ (showElement "buys" b) ++ "\n"
                                       ++ (showElement "coins" c) ++ "\n"
                                       ++ (showElement "deck" d) ++ "\n"
                                       ++ (showElement "hand" h) ++ "\n"
                                       ++ (showElement "plays" p2) ++ "\n"
                                       ++ (showElement "discards" d2) ++ " )"
                                    

showElement :: (Show a) => String -> a -> String
showElement label elem = "( "++label++" "++(show elem)++" )"
