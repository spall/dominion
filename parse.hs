module Parser where

import DominionState
import Text.Printf

-- code to parse a string into a game state

parseState :: String -> Either Error GameState
parseState input = parseElements (words input) >>= readState 

parseElements :: [String] -> Either Error [[String]]
parseElements tokens
  | (head tokens) == "(" = parseRestElements ([],(tail tokens))
  | otherwise            = Left (printf "Wrong format for state: expected ( got %s instead.\n" $ head tokens)

parseRestElements :: ([[String]],[String]) -> Either Error [[String]]
parseRestElements (elems,tokens)
  | (head tokens) == ")" = Right elems
  | otherwise = parseElement tokens >>= (\(elem, rest_tokens) -> parseRestElements (elems ++ (elem:[]), rest_tokens))

parseElement :: [String] -> Either Error ([String], [String])
parseElement tokens
  | (head tokens) == "(" = parseRestElement ((head tokens):[], tail tokens)
  | otherwise            = Left (printf "Wrong format for element: expected ( got %s instead.\n" $ head tokens)


-- works because no nesting of parens. otherwise we would need a stack or something
parseRestElement :: ([String], [String]) -> Either Error ([String], [String])
parseRestElement (elem, []) = Left (printf "No more tokens while parsing element %s" $ unwords elem)
parseRestElement (elem, tokens)
  | (head tokens) == ")"   = Right new
  | otherwise              = parseRestElement new
  where new = (elem ++ ((head tokens):[]), tail tokens)

                                                      
  
