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

parseTreasure :: ParsecT String () Identity Treasure
parseTreasure = (try $ do { symbol "copper"; return Copper })
            <|> (try $ do { symbol "silver"; return Silver })
            <|> (try $ do { symbol "gold"; return Gold })

parseVictory :: ParsecT String () Identity Victory
parseVictory = (try $ do { symbol "estate"; return Estate })
           <|> (try $ do { symbol "duchy"; return Duchy })
           <|> (try $ do { symbol "province"; return Province })

parseAction :: ParsecT String () Identity Action
parseAction = (try $ do { symbol "mine"; return Mine })
          <|> (try $ do { symbol "cellar"; return Cellar })
          <|> (try $ do { symbol "market"; return Market })
          <|> (try $ do { symbol "remodel"; return Remodel })
          <|> (try $ do { symbol "smithy"; return Smithy })
          <|> (try $ do { symbol "village"; return Village })
          <|> (try $ do { symbol "woodcutter"; return Woodcutter })
          <|> (try $ do { symbol "workshop"; return Workshop })
          <|> (try $ do { symbol "militia"; return Militia })
          <|> (try $ do { symbol "moat"; return Moat })

parseCard :: ParsecT String () Identity Card
parseCard = (try $ do { treasure <- parseTreasure;
                        return $ T treasure })
        <|> (try $ do { victory <- parseVictory;
                        return $ V victory })
        <|> (try $ do { action <- parseAction;
                        return $ A action })

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

parseNotification :: ParsecT String () Identity Notification
parseNotification = whitespace >> ( try parseMove <|> try parseMoved <|> try parseAttacked <|> try parseDefended)

parseAttacked :: ParsecT String () Identity Notification
parseAttacked = parens $ do symbol "attacked";
                            parens $ do symbol "act";
                                        symbol "militia";
                                        return ();
                            player <- lexeme identifier;
                            state <- lexeme parseState;
                            return $ Attacked (Act Militia []) (Player player) state

parseDefended :: ParsecT String () Identity Notification -- todo
parseDefended = parens $ do symbol "defended";
                            player <- lexeme identifier;
                            defense <- lexeme parseDefense;
                            return $ Defended (Player player) defense

parseDefense :: ParsecT String () Identity Defense
parseDefense = try (parens $ do {symbol "moat";
                                 return $ CardDefense Moat })
               <|> try (parens $ do { symbol "discard";
                                      cards <- many parseCard;
                                      return $ Discard cards })
                           

parseMove :: ParsecT String () Identity Notification
parseMove = parens $ do symbol "move";
                        gs <- parseState;
                        return $ Move gs

parseMoved :: ParsecT String () Identity Notification
parseMoved = parens $ do symbol "moved";
                         player <- lexeme identifier;
                         play <- lexeme parsePlay;
                         return $ Moved (Player player) play

parsePlay :: ParsecT String () Identity Play
parsePlay = try parseActPlay
        <|> try (lexeme (parens $ do { symbol "add";
                                       treasure <- parseTreasure;
                                       return $ Add treasure } ))
        <|> try (lexeme (parens $ do { symbol "buy";
                                        card <- parseCard;
                                        return $ Buy card } ))
        <|> try (lexeme (parens $ do { symbol "clean";
                                        card <- parseCard;
                                        return $ Clean (Just card) } ))
        <|> try (lexeme (parens $ do { symbol "clean";
                                        return $ Clean Nothing } ))

parseActPlay :: ParsecT String () Identity Play
parseActPlay = try (lexeme (parens $ do {symbol "act";
                                         symbol "mine";
                                         t1 <- parseTreasure;
                                         t2 <- parseTreasure;
                                         return $ Act Mine [(T t1), (T t2)] }))
               <|> try (lexeme (parens $ do { symbol "act";
                                          symbol "cellar";
                                          cards <- many parseCard;
                                          return $ Act Cellar cards }))
               <|> try (lexeme (parens $ do { symbol "act";
                                              symbol "market";
                                              return $ Act Market [] }))
               <|> try (lexeme (parens $ do { symbol "act";
                                              symbol "remodel";
                                              c1 <- parseCard;
                                              c2 <- parseCard;
                                              return $ Act Remodel [c1, c2] }))
               <|> try (lexeme (parens $ do { symbol "act";
                                              symbol "smithy";
                                              return $ Act Smithy [] }))
               <|> try (lexeme (parens $ do { symbol "act";
                                              symbol "village";
                                              return $ Act Village [] }))
               <|> try (lexeme (parens $ do { symbol "act";
                                              symbol "woodcutter";
                                              return $ Act Woodcutter [] }))
               <|> try (lexeme (parens $ do { symbol "act";
                                              symbol "workshop";
                                              card <- parseCard;
                                              return $ Act Workshop [card] }))
               <|> try (lexeme (parens $ do { symbol "act";
                                              symbol "militia";
                                              return $ Act Militia [] }))
               <|> try (lexeme (parens $ do { symbol "act";
                                              symbol "moat";
                                              return $ Act Moat [] }))

