module DominionState where

import Text.Printf
import Data.Either
import Data.Char

type Error = String

data Card = Treasure { value::Int
                     , cost::Int
                     }
           | Victory { points::Int
                     , cost::Int
                     }
           | Action { act::(GameState -> GameState)
                    }

copper :: Card
copper = Treasure 1 0

silver :: Card
silver = Treasure 2 3

gold :: Card
gold = Treasure 3 6

readTreasure :: String -> Either String Card
readTreasure str
  | str == "copper" = Right copper
  | str == "silver" = Right silver
  | str == "gold"   = Right gold
  | otherwise       = Left (printf "Expected copper silver or gold, received %s instead.\n" str)

estate :: Card
estate = Victory 1 2

duchy :: Card
duchy = Victory 3 5

province :: Card
province = Victory 6 8

curse :: Card
curse = Victory (-1) 0

readVictory :: String -> Either Error Card
readVictory str
  | str == "estate"   = Right estate
  | str == "duchy"    = Right duchy
  | str == "province" = Right province
  | otherwise         = Left (printf "Error: expected estate duchy or province, received %s instead.\n" str)
                       
readAction :: String -> Either Error Card
readAction str
  | str == "mine" = Right $ Action (\x -> x)
  | otherwise     = Left (printf "Error: expected valid action, received %s instead.\n" str)

-- error messages from read various cards are ignored.
readCard :: String -> Either Error Card
readCard str
  | isRight $ readTreasure str = readTreasure str
  | isRight $ readVictory str  = readVictory str
  | isRight $ readAction str   = readAction str
  | otherwise                  = Left (printf "Error: expected valid card, received %s instead.\n" str)


-- game state

data GameState = GameState { players :: [String]
                           , supply :: [Card]
                           , trash :: [Card]
                           , actions :: Int
                           , buys :: Int
                           , coins :: Int
                           , deck :: [Card]
                           , hand :: [Card]
                           , plays :: [Card]
                           , discards :: [Card]
                           }

readState :: [[String]] -> Either Error GameState
readState elements
  | length elements == 10 = Left (printf "Expected state with 10 elements, received %d elements.\n" $ length elements)
  | otherwise             = do players <- readStringsElement "players" (head elements)
                               supply  <- readCardsElement "supply" (head $ tail elements)
                               trash   <- readCardsElement "trash" (head $ drop 2 elements)
                               actions <- readIntElement "actions" (head $ drop 3 elements)
                               buys    <- readIntElement "buys" (head $ drop 4 elements)
                               coins   <- readIntElement "coins" (head $ drop 5 elements)
                               deck    <- readCardsElement "deck" (head $ drop 6 elements)
                               hand    <- readCardsElement "hand" (head $ drop 7 elements)
                               plays   <- readCardsElement "plays" (head $ drop 8 elements)
                               discards <- readCardsElement "discards" (head $ drop 9 elements)
                               Right $ GameState players supply trash actions buys coins deck hand plays discards


-- Code to read elements -- 

readCardsElement :: String -> [String] -> Either Error [Card]
readCardsElement elem_type elem = readElement
                                   elem_type
                                   elem
                                   (\list ->
                                     foldl (\prev e ->
                                             prev >>= (\cards ->  -- prev is not an Error
                                                        readCard e >>= (\card ->  -- card is not an Error
                                                                         return (cards ++ (card:[]))))) (Right []) list)

readStringsElement :: String -> [String] -> Either Error [String]
readStringsElement elem_type elem = readElement
                                      elem_type
                                      elem
                                      return -- Right

readIntElement :: String -> [String] -> Either Error Int
readIntElement elem_type elem = readElement
                                 elem_type
                                 elem
                                 (\rest_elem -> if (length rest_elem) /= 1
                                                then Left (printf "Error: expected element with 1 number, received element with %d values.\n" $ length rest_elem)
                                                else if not $ all isDigit (head rest_elem)
                                                     then Left (printf "Error: expected number, received %s instead.\n" $ head rest_elem)
                                                     else Right (read (head rest_elem)::Int))


readElement :: String -> [String] -> ([String] -> Either Error a) -> Either Error a
readElement elem_type elem func
  | (head elem) /= elem_type = Left (printf "Error: expected element type %s, received %s instead.\n" elem_type $ head elem)
  | otherwise                = func (tail elem)

