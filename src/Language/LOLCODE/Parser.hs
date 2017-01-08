module Language.LOLCODE.Parser where

import           Text.Parsec
import           Text.Parsec.String      (Parser)

import qualified Text.Parsec.Char        (newline)
import qualified Text.Parsec.Expr        as Ex
import qualified Text.Parsec.Token       as Tok

import           Language.LOLCODE.Lexer
import           Language.LOLCODE.Syntax

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

table1 = [[binary "*" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]]

table = []

factor :: Parser Expr
factor = try numbar
      <|> try numbr
      <|> try yarn
      <|> try function
      <|> try something
      <|> variable
      <|> parens expr

numbr :: Parser Expr
numbr = do
  n <- integer
  return $ Numbr n

numbar :: Parser Expr
numbar = do
  n <- float
  return $ Numbar n

yarn :: Parser Expr
yarn = do
  s <- stringLiteral
  return $ Yarn s

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

something :: Parser Expr
something = do
  reserved "HOW IZ I"
  v <- integer
  reserved "IF U SAY SO"
  return $ Numbr v

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many variable
  body <- expr
  return $ Function name args body

statement :: Parser Stmt
statement =  parens statement
         <|> sequenceOfStmt

sequenceOfStmt :: Parser Stmt
sequenceOfStmt = do
    list <- many $ do
        st <- statement'
        return $ st
    return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =  try assignStmt
          <|> try ifStmt
          <|> try exprStmt

assignStmt :: Parser Stmt
assignStmt = do
    name <- identifier
    reserved "R"
    value <- expr
    return $ Assign name value

ifStmt :: Parser Stmt
ifStmt = do
    reserved "O RLY?"
    reserved "YA RLY"
    yes <- statement
    maybes <- many $ do
        reserved "MEBBE"
        ex <- expr
        st <- statement
        return $ (ex, st)
    reserved "NO WAI"
    no <- statement
    reserved "OIC"
    return $ If yes maybes no

exprStmt :: Parser Stmt
exprStmt = do
    ex <- expr
    return $ ExprStmt ex

defn :: Parser Expr
defn =  try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s

parseToplevelStmt :: String -> Either ParseError Stmt
parseToplevelStmt s = parse (contents statement) "<stdin>" s
