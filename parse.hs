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
               , P.identStart     = letter
               , P.identLetter    = alphaNum
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

parseCard :: ParsecT String () Identity Card
parseCard = do { symbol "copper"; return copper }
          <|> do { symbol "silver"; return silver }
          <|> do { symbol "gold"; return gold }
          <|> do { symbol "estate"; return estate }
          <|> do { symbol "duchy"; return duchy }
          <|> do { symbol "province"; return province }
          <|> do { symbol "mine"; return mine }

parsePlayer :: ParsecT String () Identity Player
parsePlayer = do name <- many1 letter
                 return $ Player name

parsePlayers :: ParsecT String () Identity [Player]
parsePlayers = parens $ do symbol "players";
                           many (lexeme parsePlayer)

parseInteger :: String -> Parsec String () Integer
parseInteger str = parens $ do symbol str
                               integer

parseCards :: String -> Parsec String () [Card]
parseCards str = parens $ do symbol str
                             many parseCard

parseState :: Parsec String () GameState
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
