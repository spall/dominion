module Parser where

import DominionState
import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.Char
import qualified Text.Parsec.Token as P

-- define dominion state "lang"

dominionLang :: P.LanguageDef st
dominionLang = P.LanguageDef
               { P.commentStart   = ""
               , P.commentEnd     = ""
               , P.commentLine    = ""
               , P.nestedComments = False
               , P.identStart     = letter <|> P.identLetter dominionLang
               , P.identLetter    = alphaNum <|> oneOf "_-'"
               , P.opStart        = P.opLetter dominionLang
               , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , P.reservedNames= []
               , P.reservedOpNames  = []
               , P.caseSensitive  = True
               }

lexer = P.makeTokenParser dominionLang

symbol = P.symbol lexer
parens = P.parens lexer
lexeme = P.lexeme lexer
integer = P.integer lexer
identifier = P.identifier lexer
whitespace = P.whiteSpace lexer

parseTreasure :: ParsecT String () Identity Card
parseTreasure = do { symbol "copper"; return (T Copper) }
            <|> do { symbol "silver"; return (T Silver) }
            <|> do { symbol "gold"; return (T Gold) }

parseVictory :: ParsecT String () Identity Card
parseVictory = do { symbol "estate"; return (V Estate) }
           <|> do { symbol "duchy"; return (V Duchy) }
           <|> do { symbol "province"; return (V Province) }

parseAction :: ParsecT String () Identity Card
parseAction = do { symbol "mine"; return (A Mine) }

parseCard :: ParsecT String () Identity Card
parseCard = try parseTreasure
        <|> try parseVictory
        <|> try parseAction

parsePlayer :: ParsecT String () Identity Player
parsePlayer = do name <- lexeme identifier
                 return $ Player name

parsePlayers :: ParsecT String () Identity [Player]
parsePlayers = lexeme $ parens $ do symbol "players";
                                    many parsePlayer

parseInteger :: String -> ParsecT String () Identity Integer
parseInteger str = lexeme $ parens $ do symbol str
                                        lexeme integer

parseCards :: String -> ParsecT String () Identity [Card]
parseCards str = lexeme $ parens $ do symbol str
                                      many parseCard -- no lexeme needed matching symbols

parseState :: ParsecT String () Identity GameState
parseState = lexeme $ parens $ do p  <- parsePlayers
                                  s  <- parseCards "supply"
                                  t  <- parseCards "trash"
                                  a  <- parseInteger "actions"
                                  b  <- parseInteger "buys"
                                  c  <- parseInteger "coins"
                                  d  <- parseCards "deck"
                                  h  <- parseCards "hand"
                                  p2 <- parseCards "plays"
                                  d2 <- parseCards "discards"
                                  return $ GameState p s t a b c d h p2 d2

parseNotification :: ParsecT String () Identity (Maybe GameState)
parseNotification = whitespace >> ( try parseMove <|> try parseMoved )

parseMove :: ParsecT String () Identity (Maybe GameState)
parseMove = parens $ do symbol "move";
                        gs <- parseState;
                        return $ Just gs

parseMoved :: ParsecT String () Identity (Maybe GameState)
parseMoved = parens $ do symbol "moved";
                         lexeme identifier;
                         lexeme parsePlay;
                         return Nothing

parsePlay :: ParsecT String () Identity ()
parsePlay = try (lexeme (parens $ do { symbol "act";
                                        symbol "mine";
                                        parseTreasure;
                                        parseTreasure;
                                        return () } ))
        <|> try (lexeme (parens $ do { symbol "add";
                                        parseTreasure;
                                        return () } ))
        <|> try (lexeme (parens $ do { symbol "buy";
                                        parseCard;
                                        return () } ))
        <|> try (lexeme (parens $ do { symbol "clean";
                                        parseCard;
                                        return () } ))
        <|> try (lexeme (parens $ do { symbol "clean";
                                        return () } ))
