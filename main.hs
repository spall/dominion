module Main where

import Parser
import Text.Parsec.String

main = do result <- parseFromFile parseState "test.txt"
          case result of
            Left err -> putStrLn $ show err
            Right xs -> putStrLn $ show xs
