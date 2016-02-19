module Player1 where

import DominionState

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

countMoney :: [Card] -> Integer
countMoney ls = foldl (\sum card -> case card of
                                    T treasure -> sum + (value treasure)
                                    _          -> sum) 0 ls

-- bug with counting money from hand?
-- depending on what server does that money gets reused. not allwoed
tryBuy :: GameState -> Maybe String
tryBuy GameState{ buys = b
                , coins = c
                , hand = h
                , supply = s}
  | b < 1 = Nothing
  | money > (cost Province)
    && (elem (V Province) s) = Just $ unwords ["(", "buy", show Province, ")"]
  | money > (cost Mine)
    && (elem (A Mine) s) = Just $ unwords ["(", "buy", show Mine, ")"]
  | money > (cost Gold)
    && (elem (T Gold) s) = Just $ unwords ["(", "buy", show Gold, ")"]
  | money > (cost Silver)
    && (elem (T Silver) s) = Just $ unwords ["(", "buy", show Silver, ")"]
  | otherwise = Nothing
  where money = (countMoney h) + c

doTurn :: GameState -> String
doTurn gs = case tryAction gs of
            (Just str) -> str
            Nothing -> case tryBuy gs of
                       (Just str2) -> str2
                       Nothing -> case hand gs of
                                  [] -> unwords ["(", "clean", ")"]
                                  (f:r) -> unwords ["(", "clean", show f, ")"]
                                  
