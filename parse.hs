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
parseCard = parseTreasure
        <|> parseVictory
        <|> parseAction

parsePlayer :: ParsecT String () Identity Player
parsePlayer = do name <- identifier
                 return $ Player name

parsePlayers :: ParsecT String () Identity [Player]
parsePlayers = parens $ do symbol "players";
                           many (lexeme parsePlayer)

parseInteger :: String -> ParsecT String () Identity Integer
parseInteger str = parens $ do symbol str
                               integer

parseCards :: String -> ParsecT String () Identity [Card]
parseCards str = parens $ do symbol str
                             many parseCard

parseState :: ParsecT String () Identity GameState
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

parseNotification :: ParsecT String () Identity (Maybe GameState)
parseNotification = whitespace >> (parens $ do { symbol "move";
                                                 gs <- parseState;
                                                 return $ Just gs })
                       
parseWord :: ParsecT String () Identity String
parseWord = symbol "move" >> (return "yes")

parsePlay :: ParsecT String () Identity ()
parsePlay = (parens $ do { symbol "act";
                           symbol "mine";
                           lexeme $ parseTreasure;
                           lexeme $ parseTreasure;
                           return () } )
        <|> (parens $ do { symbol "add";
                           lexeme $ parseTreasure;
                           return () } )
        <|> (parens $ do { symbol "buy";
                           lexeme $ parseCard;
                           return () } )
        <|> (parens $ do { symbol "clean";
                           lexeme $ parseCard;
                           return () } )
        <|> (parens $ do { symbol "clean";
                           return () } )
