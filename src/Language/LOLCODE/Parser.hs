module Language.LOLCODE.Parser where

import           Text.Parsec
import           Text.Parsec.String      (Parser)

import qualified Text.Parsec.Char        (newline)
import qualified Text.Parsec.Expr        as Ex
import qualified Text.Parsec.Token       as Tok

import           Language.LOLCODE.Lexer
import           Language.LOLCODE.Syntax

import           Control.Monad           (liftM)
import           Data.List.Utils         (replace)

-- Input will be canonicalized by appending this symbol to end of each line
lineEndSymbol = ","

-- Should be used explicitly when parsing multi-line expressions or statements
lineEnd :: Parser ()
lineEnd = optional . many . symbol $ lineEndSymbol

escapeYarn :: String -> String
escapeYarn = f1 . f2 . f3 . f4 . f5
    where
        f1 = replace ":)" "\n"
        f2 = replace ":>" "\t"
        --f3 = replace ":o" "\g"
        f3 = id
        f4 = replace ":\"" "\""
        f5 = replace "::" ":"

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
numbar =  (symbol "-" >> float >>= \x -> return $ Numbar ((-1) * x))
      <|> (float >>= \x -> return $ Numbar x)

troof :: Parser Expr
troof = do
    s <- try (symbol "WIN") <|> try (symbol "FAIL")
    case s of
        "WIN" -> return $ Troof True
        _ -> return $ Troof False

yarn :: Parser Expr
yarn = do
    s <- stringLiteral
    return $ Yarn (escapeYarn s)

vtype :: Parser Type
vtype =  try (symbol "TROOF" >> return TroofT)
     <|> try (symbol "NUMBR" >> return NumbrT)
     <|> try (symbol "NUMBAR" >> return NumbarT)
     <|> try (symbol "YARN" >> return YarnT)
     <|> try (symbol "NOOB" >> return NoobT)

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
    op <- try (symbol "SUM OF"  >> return Sum)
      <|> try (symbol "DIFF OF" >> return Diff)
      <|> try (symbol "PRODUKT OF" >> return Produkt)
      <|> try (symbol "QUOSHUNT OF" >> return Quoshunt)
      <|> try (symbol "MOD OF" >> return Mod)
      <|> try (symbol "BIGGR OF" >> return Biggr)
      <|> try (symbol "SMALLR OF" >> return Smallr)
      <|> try (symbol "BOTH OF" >> return Both)
      <|> try (symbol "EITHER OF" >> return Either)
      <|> try (symbol "WON OF" >> return Won)
      <|> try (symbol "BOTH SAEM" >> return Saem)
      <|> try (symbol "DIFFRINT" >> return Diffrint)
    a <- expr
    optional $ reserved "AN"
    b <- expr
    return $ BinOp op a b

naryOp :: Parser Expr
naryOp = do
    op <- try (symbol "ALL" >> return All)
      <|> try (symbol "ANY" >> return Any)
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

stmtContext :: Parser StmtContext
stmtContext = do
    pos <- getPosition
    return StmtContext { filename = sourceName pos
                       , line_number = sourceLine pos }

statement :: Parser Stmt
statement = sequenceOfStmt

sequenceOfStmt :: Parser Stmt
sequenceOfStmt = do
    list <- many $ do
        pos <- getPosition
        let tag = StmtContext { filename = sourceName pos
                              , line_number = sourceLine pos }
        st <- statement'
        lineEnd
        return $ Tagged st tag
    return $ if length list == 1 then head list else Seq list

untagPairs :: [(Expr, Stmt)] -> [(Expr, Stmt)]
untagPairs = map (\x -> (fst x, untag $ snd x))

untagMaybe :: Maybe Stmt -> Maybe Stmt
untagMaybe = fmap untag

untagExpr :: Expr -> Expr
untagExpr (Function name args prog) = Function name args (untag prog)
untagExpr p@_ = p

untag :: Stmt -> Stmt
untag (Seq sts) = Seq $ map untag sts
untag (Tagged (If yes maybes no) _) = If (untag yes) (untagPairs maybes) (untagMaybe no)
untag (Tagged (Case options defc) _) = Case (untagPairs options) (untagMaybe defc)
untag (Tagged (Loop label lop lcond prog) _) = Loop label lop lcond (untag prog)
untag (Tagged st _) = untag st
untag p@_ = p

statement' :: Parser Stmt
statement' =  parens statement'
          <|> try assignStmt
          <|> try declareStmt
          <|> try castStmt1
          <|> try castStmt2
          <|> try ifStmt
          <|> try exprStmt
          <|> try printStmt
          <|> try readStmt
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
        Just _ -> liftM (Declare name) expr

castStmt1 :: Parser Stmt
castStmt1 = do
    name <- identifier
    reserved "IS NOW A"
    tp <- vtype
    return $ Cast2 name tp

castStmt2 :: Parser Stmt
castStmt2 = do
    name <- identifier
    reserved "R"
    optional (reserved "MAEK")
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
        return (ex, st)
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

readStmt :: Parser Stmt
readStmt = do
    reserved "GIMMEH"
    name <- identifier
    return $ Read name

returnStmt :: Parser Stmt
returnStmt = liftM Return (reserved "FOUND YR" >> expr)

breakStmt :: Parser Stmt
breakStmt = reserved "GTFO" >> return Break

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
loopOp =  liftM Increment (try (reserved "UPPIN" >> reserved "YR" >> identifier))
      <|> liftM Decrement (try (reserved "NERFIN" >> reserved "YR" >> identifier))
      <|> try (do
            reserved "I IZ"
            f <- identifier
            reserved "YR"
            v <- identifier
            reserved "MKAY"
            return $ UFunc f v)
      <|> return Noop

loopCond :: Parser LoopCond
loopCond =  liftM Until (try (reserved "TIL") >> expr)
        <|> liftM While (try (reserved "WILE") >> expr)
        <|> return Forever

caseStmt :: Parser Stmt
caseStmt = do
    reserved "WTF?" >> lineEnd
    conds <- many (do
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

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    lineEnd  -- in case a canonicalized new line appears before the very first statement
    reserved "HAI" >> float >> lineEnd
    r <- p
    reserved "KTHXBYE" >> lineEnd
    eof
    return r

canonicalize :: String -> String
canonicalize = replace "\n" (lineEndSymbol ++ "\n")

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents $ many expr) "<stdin>"

parseToplevelStmtWithFilename :: String -> String -> Either ParseError Stmt
parseToplevelStmtWithFilename filename s = parse (contents statement) filename (canonicalize s)

parseToplevelStmt :: String -> Either ParseError Stmt
parseToplevelStmt = parseToplevelStmtWithFilename "<stdin>"
