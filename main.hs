module Main where

import Parser
import DominionState
import Text.Parsec.Prim
import Text.Parsec.String
import Player1

helper :: (Maybe GameState) -> IO String
helper Nothing = putStrLn "nothing" >> loop
helper (Just gs) = (putStrLn $ doTurn gs) >> loop

loop :: IO String
loop = do notification <- getContents
          case (runP parseNotification () "stdin" notification) of
            (Left err) -> putStrLn "error:" >> (return $ show err)
            (Right mgs) -> putStrLn "calling helper:" >> (helper mgs)

loop2 :: IO String
loop2 = do str <- getContents
           case (runP parseWord () "stdin" str) of
             (Left err) -> putStrLn "error:" >> (return $ show err)
             (Right mgs) -> putStrLn "calling helper:" >> (return $ show mgs)

main = do notification <- getLine
          case (parse parseNotification "stdin" notification) of
            (Left err) -> putStrLn "error:" >> (return $ show err)
            (Right mgs) -> case mgs of
                           Nothing -> putStrLn "nothing" >> main
                           (Just gs) -> putStrLn "doing turn" >> (putStrLn $ doTurn gs) >> main


--main = loop >>= putStrLn

  --loop
  {-do result <- parseFromFile parseNotification "notification_test.txt";
          case result of
            Left err -> print err
            Right xs -> print xs -}
  --putStrLn "starting:" >> loop >>= putStrLn
