module Language.LOLCODE.Parser where

import           Text.Parsec
import           Text.Parsec.String      (Parser)

import qualified Text.Parsec.Char        (newline)
import qualified Text.Parsec.Expr        as Ex
import qualified Text.Parsec.Token       as Tok

import           Language.LOLCODE.Lexer
import           Language.LOLCODE.Syntax

import           Data.List.Utils         (replace)

-- Input will be canonicalized by appending this symbol to end of each line
lineEndSymbol = ","

-- Should be used explicitly when parsing multi-line expressions or statements
lineEnd :: Parser ()
lineEnd = optional . many . reserved $ lineEndSymbol

factor :: Parser Expr
factor = try numbar
      <|> try numbr
      <|> try troof
      <|> try yarn
      <|> try debugFail
      <|> try function
      <|> try call
      <|> try variable
      <|> try cast
      <|> try smoosh
      <|> try binaryOp
      <|> try naryOp
      <|> try notOp
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

troof :: Parser Expr
troof = do
    s <- (try $ symbol "WIN") <|> (try $ symbol "FAIL")
    case s of
        "WIN" -> return $ Troof True
        _ -> return $ Troof False

yarn :: Parser Expr
yarn = do
    s <- stringLiteral
    return $ Yarn s

vtype :: Parser Type
vtype =  (try $ symbol "TROOF" >> return TroofT)
     <|> (try $ symbol "NUMBR" >> return NumbrT)
     <|> (try $ symbol "NUMBAR" >> return NumbarT)
     <|> (try $ symbol "YARN" >> return YarnT)
     <|> (try $ symbol "NOOB" >> return NoobT)

cast :: Parser Expr
cast = do
    reserved "MAEK"
    ex <- expr
    optional (reserved "A")
    tp <- vtype
    return $ Cast ex tp

variable :: Parser Expr
variable = do
    var <- identifier
    return $ Var var

function :: Parser Expr
function = do
    reserved "HOW IZ I" >> lineEnd
    name <- identifier
    args <- sepBy (reserved "YR" >> identifier) (reserved "AN")
    lineEnd
    body <- statement
    reserved "IF U SAY SO" >> lineEnd
    return $ Function name args body

call :: Parser Expr
call = do
    reserved "I IZ"
    name <- identifier
    exprs <- sepBy (reserved "YR" >> expr) (reserved "AN")
    reserved "MKAY"
    return $ Call name exprs

smoosh :: Parser Expr
smoosh = do
    reserved "SMOOSH"
    exprs <- sepBy expr (optional $ reserved "AN")
    optional $ reserved "MKAY"
    return $ Smoosh exprs

binaryOp :: Parser Expr
binaryOp = do
    op <- (try $ symbol "SUM OF"  >> return Sum)
      <|> (try $ symbol "DIFF OF" >> return Diff)
      <|> (try $ symbol "PRODUKT OF" >> return Produkt)
      <|> (try $ symbol "QUOSHUNT OF" >> return Quoshunt)
      <|> (try $ symbol "MOD OF" >> return Mod)
      <|> (try $ symbol "BIGGR OF" >> return Biggr)
      <|> (try $ symbol "SMALLR OF" >> return Smallr)
      <|> (try $ symbol "BOTH OF" >> return Both)
      <|> (try $ symbol "EITHER OF" >> return Either)
      <|> (try $ symbol "WON OF" >> return Won)
      <|> (try $ symbol "BOTH SAEM" >> return Saem)
      <|> (try $ symbol "DIFFRINT" >> return Diffrint)
    a <- expr
    optional $ reserved "AN"
    b <- expr
    return $ BinOp op a b

naryOp :: Parser Expr
naryOp = do
    op <- (try $ symbol "ALL" >> return All)
      <|> (try $ symbol "ANY" >> return Any)
    reserved "OF"
    exprs <- sepBy expr (optional $ reserved "AN")
    reserved "MKAY"
    return $ NaryOp op exprs

notOp :: Parser Expr
notOp = do
    reserved "NOT"
    ex <- expr
    return $ Not ex

debugFail :: Parser Expr
debugFail = do
    reserved "__DEBUGFAIL__"
    s <- stringLiteral
    return $ DebugFail s

statement :: Parser Stmt
statement =  sequenceOfStmt

sequenceOfStmt :: Parser Stmt
sequenceOfStmt = do
    list <- many $ do
        st <- statement'
        lineEnd
        return $ st
    return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =  parens statement'
          <|> try assignStmt
          <|> try declareStmt
          <|> try castStmt1
          <|> try castStmt2
          <|> try ifStmt
          <|> try exprStmt
          <|> try printStmt
          <|> try returnStmt
          <|> try loopStmt
          <|> try breakStmt
          <|> try caseStmt

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
        Nothing -> return $ Declare name Noob
        Just _ -> expr >>= (return . Declare name)

castStmt1 :: Parser Stmt
castStmt1 = do
    name <- identifier
    reserved "IS NOW A"
    tp <- vtype
    return $ Cast2 name tp

castStmt2 :: Parser Stmt
castStmt2 = do
    name <- identifier
    reserved "R MAEK"
    optional (reserved "A")
    tp <- vtype
    return $ Cast2 name tp

ifStmt :: Parser Stmt
ifStmt = do
    reserved "O RLY?" >> lineEnd
    reserved "YA RLY" >> lineEnd
    yes <- statement
    maybes <- many $ do
        reserved "MEBBE"
        ex <- expr;
        lineEnd
        st <- statement
        return $ (ex, st)
    no <- optionMaybe $ reserved "NO WAI" >> lineEnd >> statement
    reserved "OIC" >> lineEnd
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

breakStmt :: Parser Stmt
breakStmt = (reserved "GTFO") >> (return Break)

loopStmt :: Parser Stmt
loopStmt = do
    reserved "IM IN YR"
    label <- identifier
    op <- loopOp
    cond <- loopCond
    lineEnd
    prog <- statement
    reserved "IM OUTTA YR"
    label' <- identifier
    lineEnd
    -- Check that labels match?
    return $ Loop label op cond prog

loopOp :: Parser LoopOp
loopOp =  try (reserved "UPPIN" >> reserved "YR" >> identifier >>= return . Increment)
      <|> try (reserved "NERFIN" >> reserved "YR" >> identifier >>= return . Decrement)
      <|> try (identifier >>= \f -> reserved "YR" >> identifier >>= return . UFunc f)
      <|> return Noop

loopCond :: Parser LoopCond
loopCond =  ((try $ reserved "TIL") >> expr >>= return . Until)
        <|> ((try $ reserved "WILE") >> expr >>= return . While)
        <|> return Forever

caseStmt :: Parser Stmt
caseStmt = do
    reserved "WTF?" >> lineEnd
    conds <- many $ (do
        reserved "OMG"
        cond <- valueLiteral
        lineEnd
        prog <- statement
        return (cond, prog))
    p <- optionMaybe (reserved "OMGWTF" >> lineEnd >> statement)
    reserved "OIC" >> lineEnd
    return $ Case conds p

valueLiteral :: Parser Expr
valueLiteral = try numbar
            <|> try numbr
            <|> try troof
            <|> try yarn

defn :: Parser Expr
defn =  try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  lineEnd  -- in case a canonicalized new line appears before the very first statement
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    return def

canonicalize :: String -> String
canonicalize = replace "\n" (lineEndSymbol ++ "\n")

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s

parseToplevelStmtWithFilename :: String -> String -> Either ParseError Stmt
parseToplevelStmtWithFilename filename s = parse (contents statement) filename (canonicalize s)

parseToplevelStmt :: String -> Either ParseError Stmt
parseToplevelStmt s = parseToplevelStmtWithFilename "<stdin>" s
