module Language.LOLCODE.Syntax where

data Expr
    = Noob String
    | Yarn String
    | Numbr Integer
    | Numbar Double
    | Troof Bool
    | Var String
    | BinOp Op Expr Expr
    | NaryOp OpN [Expr]
    | Maek Expr Type
    | Maek2 String Type
    | Function String [String] Stmt
    | Call String [Expr]
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
    | Not
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
    | Print String
    | Declare String
    | Assign String Expr
    | Return Expr
    | If Stmt [(Expr, Stmt)] Stmt
    | Case Stmt [(Expr, Stmt)] Stmt
    -- TDOO: Loop
    | CaseGtfo
    | Found Expr
    | FunctionGtfo
    | ExprStmt Expr
    deriving (Eq, Ord, Show)
