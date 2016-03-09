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
       && (elem (T Gold) s) = Just $ unwords ["(", "act", show Mine, show Silver, show Gold, ")"]
                              
  |    (elem (A Mine) h)
       && (elem (T Copper) h)
       && (elem (T Silver) s) = Just $ unwords ["(", "act", show Mine, show Copper, show Silver, ")"]
                                                                             
  | otherwise = Nothing

{-
 Cellar – You can't discard Cellar to itself, since it isn't in your
hand any longer when you resolve it. You choose what cards to
discard and discard them all at once. You only draw cards after
you have discarded. If you have to shuffle to do the drawing, the
discarded cards will end up shuffled into your new Deck.
-}
tryCellar :: GameState -> Maybe String
tryCellar gs = Nothing -- todo

tryMarket :: GameState -> Maybe String
tryMarket GameState{hand=h}
  | (elem (A Market) h) = Just $ unwords ["(", "act", show Market, ")"]
  | otherwise = Nothing

tryRemodel :: GameState -> Maybe String
tryRemodel gs = Nothing -- todo

trySmithy :: GameState -> Maybe String
trySmithy GameState{hand=h}
  | (elem (A Smithy) h) = Just $ unwords ["(", "act", show Smithy, ")"]
  | otherwise = Nothing

tryVillage :: GameState -> Maybe String
tryVillage GameState{hand=h}
  | (elem (A Village) h) = Just $ unwords ["(", "act", show Village, ")"]
  | otherwise = Nothing

tryWoodcutter :: GameState -> Maybe String
tryWoodcutter GameState{hand=h}
  | (elem (A Woodcutter) h) = Just $ unwords ["(", "act", show Woodcutter, ")"]
  | otherwise = Nothing

-- must cost no more than 4
tryWorkshop :: GameState -> Maybe String 
tryWorkshop GameState{hand=h
                      ,supply=s}
  | not (elem (A Workshop) h) = Nothing
  | (elem (A Remodel) s) = reply (A Remodel)
  | (elem (A Smithy) s) = reply (A Smithy)
  | (elem (A Village) s) = reply (A Village)
  | (elem (A Woodcutter) s) = reply (A Woodcutter)
  | (elem (A Workshop) s) = reply (A Workshop)
  | (elem (T Silver) s) = reply (T Silver)
  | otherwise = Nothing
  where reply = (\card -> Just $ unwords ["(", "act", show Workshop, show card, ")"])

tryAction :: GameState -> Maybe String
tryAction gs
  | (actions gs) < 1 = Nothing
  | otherwise = case filter (\result -> isJust result)
                     (map (\f -> f gs) fActions) of
                []    -> Nothing
                (f:r) -> f
  where fActions = [tryMine, tryCellar, tryMarket
                  , tryRemodel, trySmithy, tryVillage
                  , tryWoodcutter, tryWorkshop]


firstTreasure :: [Card] -> Maybe Card
firstTreasure ls = case filter (\c -> c == (T Copper)
                                      || c == (T Silver)
                                      || c == (T Gold)) ls of
                   []    -> Nothing
                   (f:r) -> Just f

tryBuy :: GameState -> Maybe String
tryBuy GameState{ buys = b
                , coins = c
                , hand = h
                , supply = s}
  | b < 1 = Nothing
            
  | isJust ft = Just $ unwords ["(add", (show $ fromJust ft)++")"]
                
  | c > (cost Province)
    && (elem (V Province) s) = reply (V Province)

  | c > (cost Gold)
    && (elem (T Gold) s) = reply (T Gold)
                         
  | c > (cost Mine)
    && (elem (A Mine) s) = reply (A Mine)
                                              
  | c > (cost Silver)
    && (elem (T Silver) s) = reply (T Silver)
                             
  | otherwise = Nothing
  where ft = firstTreasure h
        reply = (\card -> Just $ unwords ["(", "buy", show card, ")"])


doTurn :: GameState -> String
doTurn gs = case tryAction gs of
            (Just str) -> str
            Nothing -> case tryBuy gs of
                       (Just str2) -> str2
                       Nothing -> case hand gs of
                                  [] -> unwords ["(", "clean", ")"]
                                  (f:r) -> unwords ["(", "clean", show f, ")"]
                                  
