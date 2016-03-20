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
parseTreasure = (try $ do { symbol "copper"; return (T Copper) })
            <|> (try $ do { symbol "silver"; return (T Silver) })
            <|> (try $ do { symbol "gold"; return (T Gold) })

parseVictory :: ParsecT String () Identity Card
parseVictory = (try $ do { symbol "estate"; return (V Estate) })
           <|> (try $ do { symbol "duchy"; return (V Duchy) })
           <|> (try $ do { symbol "province"; return (V Province) })

parseAction :: ParsecT String () Identity Card
parseAction = (try $ do { symbol "mine"; return (A Mine) })
          <|> (try $ do { symbol "cellar"; return (A Cellar) })
          <|> (try $ do { symbol "market"; return (A Market) })
          <|> (try $ do { symbol "remodel"; return (A Remodel) })
          <|> (try $ do { symbol "smithy"; return (A Smithy) })
          <|> (try $ do { symbol "village"; return (A Village) })
          <|> (try $ do { symbol "woodcutter"; return (A Woodcutter) })
          <|> (try $ do { symbol "workshop"; return (A Workshop) })
          <|> (try $ do { symbol "militia"; return (A Militia) })
          <|> (try $ do { symbol "moat"; return (A Moat) })

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
parseNotification = whitespace >> ( try parseMove <|> try parseMoved <|> try parseAttacked <|> try parseDefended)

parseAttacked :: ParsecT String () Identity (Maybe GameState)
parseAttacked = parens $ do symbol "attacked";
                            parens $ do symbol "act";
                                        symbol "militia";
                                        return ();
                            lexeme identifier;
                            lexeme parseState;
                            return Nothing

parseDefended :: ParsecT String () Identity (Maybe GameState)
parseDefended = parens $ do symbol "defended";
                            lexeme identifier;
                            lexeme parseDefense;
                            return Nothing

parseDefense :: ParsecT String () Identity () -- don't care so return void
parseDefense = try (parens $ do {symbol "moat";
                                 return ()})
               <|> try (parens $ do { symbol "discard";
                                      many parseCard;
                                      return ()})
                           

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
parsePlay = try parseActPlay
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

parseActPlay = try (lexeme (parens $ do {symbol "act";
                                         symbol "mine";
                                         parseTreasure;
                                         parseTreasure;
                                         return () }))
               <|> try (lexeme (parens $ do { symbol "act";
                                          symbol "cellar";
                                          many parseCard;
                                          return () }))
               <|> try (lexeme (parens $ do { symbol "act";
                                              symbol "market";
                                              return () }))
               <|> try (lexeme (parens $ do { symbol "act";
                                              symbol "remodel";
                                              parseCard;
                                              parseCard;
                                              return () }))
               <|> try (lexeme (parens $ do { symbol "act";
                                              symbol "smithy";
                                              return () }))
               <|> try (lexeme (parens $ do { symbol "act";
                                              symbol "village";
                                              return () }))
               <|> try (lexeme (parens $ do { symbol "act";
                                              symbol "woodcutter";
                                              return () }))
               <|> try (lexeme (parens $ do { symbol "act";
                                              symbol "workshop";
                                              parseCard;
                                              return () }))

