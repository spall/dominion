module Player1 where

import DominionState
import Data.Maybe
import Data.List

--(act mine treasure treasure)
tryMine :: GameState -> Maybe Play
tryMine GameState{ supply = s
                 , hand   = h}
  |    (elem (A Mine) h)
       && (elem (T Silver) h)
       && (elem (T Gold) s) = Just $ Act Mine [(T Silver), (T Gold)]
                              
  |    (elem (A Mine) h)
       && (elem (T Copper) h)
       && (elem (T Silver) s) = Just $ Act Mine [(T Copper), (T Silver)]
                                                                             
  | otherwise = Nothing

{-
 Cellar – You can't discard Cellar to itself, since it isn't in your
hand any longer when you resolve it. You choose what cards to
discard and discard them all at once. You only draw cards after
you have discarded. If you have to shuffle to do the drawing, the
discarded cards will end up shuffled into your new Deck.
-}
tryCellar :: GameState -> Maybe Play
tryCellar GameState{hand=h}
  | not (elem (A Cellar) h) = Nothing
  | otherwise               = Just $ Act Cellar (filter (\card -> case card of
                                                                  (V anything) -> True
                                                                  _            -> False) h)
                          
tryMarket :: GameState -> Maybe Play
tryMarket GameState{hand=h}
  | (elem (A Market) h) = Just $ Act Market []
  | otherwise           = Nothing

tryRemodel :: GameState -> Maybe Play
tryRemodel GameState{hand=h
                    , supply=s}
  | not (elem (A Remodel) h) = Nothing
  | (elem (A Workshop) h) && (elem (A Market) s) = Just $ Act Remodel [(A Workshop), (A Market)]
  | (elem (A Workshop) h) && (elem (A Mine) s)   = Just $ Act Remodel [(A Workshop), (A Mine)]
  | otherwise                                    = Nothing

trySmithy :: GameState -> Maybe Play
trySmithy GameState{hand=h}
  | (elem (A Smithy) h) = Just $ Act Smithy []
  | otherwise           = Nothing

tryVillage :: GameState -> Maybe Play
tryVillage GameState{hand=h}
  | (elem (A Village) h) = Just $ Act Village []
  | otherwise = Nothing

tryWoodcutter :: GameState -> Maybe Play
tryWoodcutter GameState{hand=h}
  | (elem (A Woodcutter) h) = Just $ Act Woodcutter [] 
  | otherwise = Nothing

-- must cost no more than 4
tryWorkshop :: GameState -> Maybe Play 
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
  where reply = (\card -> Just $ Act Workshop [card])

tryMilitia :: GameState -> Maybe Play
tryMilitia GameState{hand=h}
  | (elem (A Militia) h) = Just $ Act Militia []
  | otherwise            = Nothing

tryMoat :: GameState -> Maybe Play
tryMoat GameState{hand=h}
  | (elem (A Moat) h) = Just $ Act Moat []
  | otherwise = Nothing

tryAction :: GameState -> Maybe Play
tryAction gs
  | (actions gs) < 1 = Nothing
  | otherwise = case filter (\result -> isJust result)
                     (map (\f -> f gs) fActions) of
                []    -> Nothing
                (f:r) -> f
  where fActions = [tryCellar, tryMine, tryMarket
                  , tryRemodel, trySmithy, tryVillage
                  , tryWoodcutter, tryWorkshop, tryMilitia]


firstTreasure :: [Card] -> Maybe Treasure
firstTreasure ls = case filter (\c -> c == (T Copper)
                                      || c == (T Silver)
                                      || c == (T Gold)) ls of
                   []    -> Nothing
                   ((T f):r) -> Just f

tryBuy :: GameState -> Maybe Play
tryBuy GameState{ buys = b
                , coins = c
                , hand = h
                , supply = s}
  | b < 1 = Nothing
            
  | isJust ft = Just $ Add (fromJust ft)
                
  | c > (cost Province)
    && (elem (V Province) s) = reply (V Province)

  | c > (cost Gold)
    && (elem (T Gold) s) = reply (T Gold)

  | c > (cost Market)
    && (elem (A Market) s) = reply (A Market)
                         
  | c > (cost Mine)
    && (elem (A Mine) s) = reply (A Mine)

  | c > (cost Silver)
    && (elem (T Silver) s) = reply (T Silver)
                             
  | otherwise = Nothing
  where ft = firstTreasure h
        reply = (\card -> Just $ Buy card)


doTurn :: GameState -> Play
doTurn gs = case tryAction gs of
            (Just play) -> play
            Nothing -> case tryBuy gs of
                       (Just play2) -> play2
                       Nothing -> case hand gs of
                                  [] -> Clean Nothing
                                  (f:r) -> Clean (Just f)

doDiscard :: GameState -> Defense
doDiscard GameState{hand=h} = Discard (drop 3 h)

doDefense :: GameState -> Defense
doDefense gs = case tryMoat gs of
               (Just str) -> CardDefense Moat
               Nothing    -> doDiscard gs
                                  
