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
    | DebugFail String
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

data StmtContext = StmtContext { filename    :: String
                               , line_number :: Int }

                 | EmptyContext
                 deriving (Eq, Ord, Show)

data Stmt
    = Seq [Stmt]
    | Tagged Stmt StmtContext
    | Print [Expr] Bool
    | Read String
    | Declare String Expr
    | Assign String Expr
    | Cast2 String Type
    | Return Expr
    | If Stmt [(Expr, Stmt)] (Maybe Stmt)
    | Case [(Expr, Stmt)] (Maybe Stmt)
    | Loop String LoopOp LoopCond Stmt
    | Break
    | ExprStmt Expr
    deriving (Eq, Ord, Show)

data LoopOp = Increment String
            | Decrement String
            | UFunc String String
            | Noop
            deriving (Eq, Ord, Show)

data LoopCond = Until Expr | While Expr | Forever deriving (Eq, Ord, Show)
