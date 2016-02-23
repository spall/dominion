module Main where

import Parser
import DominionState
import Text.Parsec.Prim
import Text.Parsec.String
import qualified Data.ByteString as B
import System.IO
import Player1

-- cannot error...
parseInput :: String -> IO (Maybe GameState)
parseInput input =  do c <- getChar
                       case (parse parseNotification "stdin" (input++(c:[]))) of
                         (Left err)  -> parseInput $ input++(c:[])
                         (Right mgs) -> return mgs

main = do result <- parseInput ""
          case result of
            Nothing -> main
            (Just gs) -> let result = doTurn gs in
                         do hPutStrLn stderr result;
                            hPutStrLn stdout result;
                            hFlush stdout;
                            main;
