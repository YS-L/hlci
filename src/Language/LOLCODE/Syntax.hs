module Language.LOLCODE.Syntax where

data Expr
    = Noob
    | Yarn String
    | Numbr Integer
    | Numbar Double
    | Troof Bool
    | Var String
    | Not Expr
    | BinOp Op Expr Expr
    | NaryOp OpN [Expr]
    | Cast Expr Type
    | Function String [String] Stmt
    | Call String [Expr]
    | Smoosh [Expr]
    deriving (Eq, Ord, Show)

data Op
    = Sum
    | Diff
    | Produkt
    | Quoshunt
    | Mod
    | Biggr
    | Smallr
    | Both
    | Either
    | Won
    | Saem
    | Diffrint
    deriving (Eq, Ord, Show)

data OpN
    = All
    | Any
    deriving (Eq, Ord, Show)

data Type
    = NoobT
    | YarnT
    | NumbrT
    | NumbarT
    | TroofT
    deriving (Eq, Ord, Show)

data Stmt
    = Seq [Stmt]
    | Print [Expr] Bool
    | Declare String Expr
    | Assign String Expr
    | Cast2 String Type
    | Return Expr
    | If Stmt [(Expr, Stmt)] Stmt
    | Case Stmt [(Expr, Stmt)] Stmt
    -- TDOO: Loop
    | Gtfo
    | ExprStmt Expr
    deriving (Eq, Ord, Show)
