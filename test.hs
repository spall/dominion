module Test where

import DominionState
import Parser
import Text.Parsec.Prim
import Control.Monad.Identity
import System.IO

testState = "((players sarah scott) (supply gold silver mine cellar market remodel smithy village woodcutter workshop militia moat) (trash gold)(actions 2) (buys 0) (coins 1) (deck gold)(hand mine cellar gold silver) (plays remodel) (discards mine woodcutter))"

testParse :: ParsecT String () Identity t -> String -> String
testParse parser input = case (parse parser "test" input) of
                         (Left err)  -> (show err)
                         (Right res) -> "Passed"

-- test action

-- militia
-- moat
	--mine
 	 --		 	cellar
 	 --		 	market
 	 --		 	remodel
 	 --		 	smithy
 	 --		 	village
 	 --		 	woodcutter
 	 --		 	workshop


-- test play

-- (act militia)
-- (act moat)
--(act cellar card ...)
 --	 		 	(act market)
 --	 		 	(act remodel card card)
 --	 		 	(act smithy)
 --	 		 	(act village)
 --	 		 	(act woodcutter)
 --	 		 	(act workshop card)
--(act mine treasure treasure)
 	 	--	 	(add treasure) ; adds coins
 	 	--	 	(buy card)
 	 	--	 	(clean) 
 	 	--	 	(clean card)

testPlay :: [String]
testPlay = map (\input -> (input ++ ": " ++ (testParse parsePlay input) ++ "\n"))
           ["(act militia)", "(act moat)", "(act cellar mine silver gold)", "(act market)"
           , "(act remodel silver duchy)", "(act smithy)", "(act village)"
           , "(act woodcutter)", "(act workshop silver)", "(act mine silver gold)"
           , "(add gold)", "(buy mine)", "(clean)", "(clean mine)"]


-- test notification

-- (attacked (act militia) name state)
-- (defended name defense)
-- (move state)
-- (moved name play)

testNotification :: [String]
testNotification = map (\input -> (input ++ ": " ++ (testParse parseNotification input) ++ "\n"))
                   ["(attacked (act militia) sarah " ++ testState ++ ")", "(defended sarah (moat))"
                   , "(move " ++ testState ++ ")", "(moved sarah (add silver))"]


-- test defense

-- (moat)
-- (discard card ...)

testDefense :: [String]
testDefense = map (\input -> (input ++ ": " ++ (testParse parseDefense input) ++ "\n"))
              ["(moat)", "(discard silver gold)"]

runTests :: IO ()
runTests = foldl (\sum input -> sum >> (hPutStrLn stderr input)) (return ()) (testPlay ++ testNotification ++ testDefense)


