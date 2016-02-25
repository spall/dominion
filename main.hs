module Main where

import AlternateParser
import Data.Attoparsec.ByteString
import DominionState
import qualified Data.ByteString.Char8 as B
import System.IO
import Player1

-- cannot error...
{-
parseInput :: String -> IO (Maybe GameState)
parseInput input =  do c <- getChar
                       case (parse parseNotification "stdin" (input++(c:[]))) of
                         (Left err)  -> parseInput $ input++(c:[])
                         (Right mgs) -> return mgs
-}

parseInputDriver :: IO (Either String (Maybe GameState))
parseInputDriver = do c <- getChar
                      case parse parseNotification $ B.pack [c] of
                        (Fail _ _ err)  -> return $ Left err
                        (Partial k)     -> parseInput k 
                        (Done _ result) -> return $ Right result

parseInput :: (B.ByteString -> IResult B.ByteString (Maybe GameState)) -> IO (Either String (Maybe GameState))
parseInput kont = do c <- getChar
                     case kont $ B.pack [c] of
                       (Fail _ _ err)  -> return $ Left err
                       (Partial k)     -> parseInput k
                       (Done _ result) -> return $ Right result
                   

main = do result <- parseInputDriver
          case result of
            (Left err) -> hPutStrLn stderr err
            (Right mgs) -> case mgs of
                           Nothing -> main
                           (Just gs) -> let result = doTurn gs in
                                        do hPutStrLn stderr result;
                                           hPutStrLn stdout result;
                                           hFlush stdout;
                                           main;
