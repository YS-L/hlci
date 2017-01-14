module Language.LOLCODE.Lexer where

import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)

import qualified Text.Parsec.Token    as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+","*","-",";"]
    names = [ "R", "AN", "BTW"
            , "SUM", "DIFF", "PRODUKT", "QUOSHUNT", "MOD", "BIGGR", "SMALLR", "OF"
            , "BOTH", "EITHER", "WON", "NOT", "ALL", "ANY"
            , "SAEM", "DIFFRINT"
            , "MAEK", "A"
            , "VISIBLE", "GIMMEH"
            , "O", "RLY", "YA", "MEBBE", "NO", "WAI", "OIC"
            , "HOW", "IZ", "I", "YR", "IF", "U", "SAY", "SO"
            , "VISIBLE"
            , "I", "HAS", "A", "ITZ"
            , "IS", "NOW", "R", "MAEK"
            , "I", "IZ"
            , "FOUND", "YR"
            ]
    style = emptyDef {
               Tok.commentLine = "#"
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
             }

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer
