module Language.LOLCODE.Syntax where

data Expr
    = Noob String
    | Yarn String
    | Numbr Integer
    | Numbar Double
    | Troof Bool
    | Var String
    | BinOp Op Expr Expr
    | Function String [Expr] Expr
    | Call String [Expr]
    deriving (Eq, Ord, Show)

data Stmt
    = Seq [Stmt]
    | Print String
    | Declare String
    | Assign String Expr
    | Return Expr
    | If Stmt [(Expr, Stmt)] Stmt
    | Break
    | ExprStmt Expr
    deriving (Eq, Ord, Show)

data Op
    = Plus
    | Minus
    | Times
    | Divide
    deriving (Eq, Ord, Show)
