module Main where

--import AlternateParser
--import Data.Attoparsec.ByteString
import Test
import Parser
import Text.Parsec.Prim
import DominionState
import qualified Data.ByteString.Char8 as B
import System.IO
import Player1

-- cannot error...

parseInput :: IO String
parseInput =  do line <- getLine
                 case (parse parseNotification "stdin" line) of
                   (Left err)  -> hPutStrLn stderr (show err) >> (hPutStrLn stdout (show err)) >> parseInput
                   (Right not) -> case not of
                                  Move{ state=gs }     -> hPutStrLn stdout (show $ doTurn gs) >> (hFlush stdout) >> parseInput
                                  Attacked{ state=gs } -> hPutStrLn stdout (show $ doDefense gs) >> (hFlush stdout) >> parseInput
                                  _ -> hPutStrLn stderr "else" >> parseInput

main = parseInput
  -- runTests
  --parseInput
