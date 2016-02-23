module Player1 where

import DominionState
import Data.Maybe
import Data.List

--(act mine treasure treasure)
tryMine :: GameState -> Maybe String
tryMine GameState{ supply = s
                 , hand   = h}
  |    (elem (A Mine) h)
    && (elem (T Silver) h)
    && (elem (T Gold) s) = Just $ unwords ["(", show Mine, show Silver, show Gold, ")"]
  |    (elem (A Mine) h)
    && (elem (T Copper) h)
    && (elem (T Silver) s) = Just $ unwords ["(", show Mine, show Copper, show Silver, ")"]
  | otherwise = Nothing

tryAction :: GameState -> Maybe String
tryAction gs
  | (actions gs) < 1 = Nothing
  | otherwise = tryMine gs

firstTreasure :: [Card] -> Maybe Card
firstTreasure ls
  | isJust firstCopper = Just $ fromJust firstCopper
  | isJust firstSilver = Just $ fromJust firstSilver
  | isJust firstGold   = Just $ fromJust firstGold
  | otherwise          = Nothing
  where firstCopper = find ((T Copper) ==) ls
        firstSilver = find ((T Silver) ==) ls
        firstGold   = find ((T Gold) ==) ls

tryBuy :: GameState -> Maybe String
tryBuy GameState{ buys = b
                , coins = c
                , hand = h
                , supply = s}
  | b < 1 = Nothing
  | isJust ft = Just $ unwords ["(add", (show $ fromJust ft)++")"]
  | c > (cost Province)
    && (elem (V Province) s) = Just $ unwords ["(", "buy", show Province, ")"]
  | c > (cost Mine)
    && (elem (A Mine) s) = Just $ unwords ["(", "buy", show Mine, ")"]
  | c > (cost Gold)
    && (elem (T Gold) s) = Just $ unwords ["(", "buy", show Gold, ")"]
  | c > (cost Silver)
    && (elem (T Silver) s) = Just $ unwords ["(", "buy", show Silver, ")"]
  | otherwise = Nothing
  where ft = firstTreasure h


doTurn :: GameState -> String
doTurn gs = case tryAction gs of
            (Just str) -> str
            Nothing -> case tryBuy gs of
                       (Just str2) -> str2
                       Nothing -> case hand gs of
                                  [] -> unwords ["(", "clean", ")"]
                                  (f:r) -> unwords ["(", "clean", show f, ")"]
                                  
