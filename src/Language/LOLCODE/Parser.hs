module Language.LOLCODE.Parser where

import           Text.Parsec
import           Text.Parsec.String      (Parser)

import qualified Text.Parsec.Char        (newline)
import qualified Text.Parsec.Expr        as Ex
import qualified Text.Parsec.Token       as Tok

import           Language.LOLCODE.Lexer
import           Language.LOLCODE.Syntax

factor :: Parser Expr
factor = try numbar
      <|> try numbr
      <|> try yarn
      <|> try function
      <|> try call
      <|> try variable
      <|> try maek
      <|> parens expr

expr :: Parser Expr
expr = Ex.buildExpressionParser [] factor

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

vtype :: Parser Type
vtype = do
    tp <- (try $ symbol "TROOF")
      <|> (try $ symbol "NUMBR")
      <|> (try $ symbol "NUMBAR")
      <|> (try $ symbol "YARN")
      <|> (try $ symbol "NOOB")
    case tp of
        "TROOF" -> return TroofT
        "NUMBR" -> return NumbrT
        "NUMBAR" -> return NumbarT
        "YARN" -> return YarnT
        "NOOB" -> return NoobT
        _ -> unexpected "Invalid type"

maek :: Parser Expr
maek = do
    reserved "MAEK"
    ex <- expr
    optional (reserved "A")
    tp <- vtype
    return $ Maek ex tp

variable :: Parser Expr
variable = do
    var <- identifier
    return $ Var var

function :: Parser Expr
function = do
    reserved "HOW IZ I"
    name <- identifier
    args <- sepBy (reserved "YR" >> identifier) (reserved "AN")
    body <- statement
    reserved "IF U SAY SO"
    return $ Function name args body

call :: Parser Expr
call = do
    reserved "I IZ"
    name <- identifier
    exprs <- sepBy (reserved "YR" >> expr) (reserved "AN")
    reserved "MKAY"
    return $ Call name exprs

statement :: Parser Stmt
statement =  parens statement
         <|> sequenceOfStmt

sequenceOfStmt :: Parser Stmt
sequenceOfStmt = do
    list <- many $ do
        st <- statement'
        optional $ symbol ","
        return $ st
    return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =  try assignStmt
          <|> try declareStmt
          <|> try castStmt1
          <|> try castStmt2
          <|> try ifStmt
          <|> try exprStmt
          <|> try printStmt
          <|> try returnStmt

assignStmt :: Parser Stmt
assignStmt = do
    name <- identifier
    reserved "R"
    value <- expr
    return $ Assign name value

declareStmt :: Parser Stmt
declareStmt = do
    reserved "I HAS A"
    name <- identifier
    initialized <- optionMaybe (reserved "ITZ")
    case initialized of
        Nothing -> return $ Declare name (Noob name)
        Just _ -> expr >>= (return . Declare name)

castStmt1 :: Parser Stmt
castStmt1 = do
    name <- identifier
    reserved "IS NOW A"
    tp <- vtype
    return $ Maek2 name tp

castStmt2 :: Parser Stmt
castStmt2 = do
    name <- identifier
    reserved "R MAEK"
    optional (reserved "A")
    tp <- vtype
    return $ Maek2 name tp

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

printStmt :: Parser Stmt
printStmt = do
    reserved "VISIBLE"
    exprs <- many1 expr
    exclaim <- optionMaybe (reserved "!")
    let newline = case exclaim of
            Just _ -> False
            Nothing -> True
    return $ Print exprs newline

returnStmt :: Parser Stmt
returnStmt = (reserved "FOUND YR") >> expr >>= (return . Return)

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
