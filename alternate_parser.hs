module AlternateParser where

import Control.Applicative
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as BC
import Data.ByteString.Char8
import DominionState

identifier :: Parser String 
identifier = (pure unpack) <*> takeWhile1 (inClass "-_a-zA-Z0-9")

lexeme :: Parser a -> Parser a
lexeme pa = pa <* BC.skipSpace

parens :: Parser a -> Parser a
parens pa = lexeme $ BC.char '('
            *> (pa <* BC.char ')')

parseVictory :: Parser Card
parseVictory = ((string $ pack "estate") *> pure (V Estate))
           <|> ((string $ pack "duchy") *> pure (V Duchy))
           <|> ((string $ pack "province") *> pure (V Province))

parseTreasure :: Parser Card
parseTreasure = ((string $ pack "copper") *> pure (T Copper))
            <|> ((string $ pack "silver") *> pure (T Silver))
            <|> ((string $ pack "gold") *> pure (T Gold))

parseAction :: Parser Card
parseAction = (string $ pack "mine") *> pure (A Mine)

parseCard :: Parser Card
parseCard = parseTreasure <|> parseVictory <|> parseAction

parsePlayer :: Parser Player
parsePlayer = (pure Player) <*> (lexeme identifier)

parsePlayers :: Parser [Player]
parsePlayers = parens $ (lexeme $ string $ pack "players")
                        *> many' parsePlayer

parseInteger :: String -> Parser Integer
parseInteger str = parens $ (lexeme $ string $ pack str)
                            *> ((pure read) <*> (lexeme $ many1' BC.digit))

parseCards :: String -> Parser [Card]
parseCards str = parens $ (lexeme $ string $ pack str)
                          *> many' (lexeme parseCard)

parseState :: Parser GameState
parseState = parens $ do p  <- lexeme parsePlayers
                         s  <- lexeme $ parseCards "supply"
                         t  <- lexeme $ parseCards "trash"
                         a  <- lexeme $ parseInteger "actions"
                         b  <- lexeme $ parseInteger "buys"
                         c  <- lexeme $ parseInteger "coins"
                         d  <- lexeme $ parseCards "deck"
                         h  <- lexeme $ parseCards "hand"
                         p2 <- lexeme $ parseCards "plays"
                         d2 <- lexeme $ parseCards "discards"
                         return $ GameState p s t a b c d h p2 d2

parsePlay :: Parser ()
parsePlay = (lexeme $ parens $ (lexeme $ string $ pack "act")
                                *> (lexeme $ string $ pack "mine")
                                *> lexeme parseTreasure
                                *> lexeme parseTreasure
                                *> pure ())
         <|> (lexeme $ parens $ (lexeme $ string $ pack "add")
                                *> lexeme parseTreasure
                                *> pure ())
         <|> (lexeme $ parens $ (lexeme $ string $ pack "buy")
                                *> lexeme parseCard
                                *> pure ())
         <|> (lexeme $ parens $ (lexeme $ string $ pack "clean")
                                *> lexeme parseCard
                                *> pure ())
         <|> (lexeme $ parens $ (lexeme $ string $ pack "clean")
                                *> pure ())

parseMove :: Parser (Maybe GameState)
parseMove = parens $ (lexeme $ string $ pack "move")
                     *> ( (pure Just) <*> (lexeme parseState))

parseMoved :: Parser (Maybe GameState)
parseMoved = parens $ (lexeme $ string $ pack "moved")
                      *> lexeme identifier
                      *> lexeme parsePlay
                      *> pure Nothing

parseNotification :: Parser (Maybe GameState)
parseNotification = BC.skipSpace *> (parseMove <|> parseMoved)
