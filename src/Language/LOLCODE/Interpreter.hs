module Language.LOLCODE.Interpreter where

import           Control.Monad.State     (StateT, get, liftIO, liftM, modify,
                                          put, runStateT)
import           Data.Char               (isDigit)
import           Data.List               (find, intercalate, nubBy)
import           Data.Maybe              (mapMaybe)
import           Data.String.Utils       (strip)
import           Language.LOLCODE.Syntax
import           Text.Printf             (printf)

type Store = [(String, Expr)]

data Env = Env { globals        :: Store
               , locals         :: Store
               , return_id      :: Int
               , return_token   :: Int
               , break_id       :: Int
               , break_token    :: Int
               , breakable      :: Bool
               , source_context :: StmtContext
               } deriving (Eq, Ord, Show)

type Interp a = StateT Env IO a

maxCallDepth = 10000
initReturnId = 1
initBreakId = 1

failMessage :: String -> Interp Expr
failMessage s = do
    env <- get
    let ctx = source_context env
        ln = line_number ctx
        fn = filename ctx
    liftIO $ fail $ "Line " ++ show ln ++ " in " ++ fn ++ ": " ++ s

lookupEnv :: (Env -> Store) -> String -> Interp Expr
lookupEnv f name = do
    env <- get
    case lookup name (f env) of
        Nothing -> failMessage $ "Unbounded variable '" ++ name ++ "'"
        Just ex -> return ex

filterLeadingDigits :: String -> String
filterLeadingDigits = (takeWhile (\x -> (isDigit x) || (x == '.'))) . strip

castStringToNumeric :: String -> Double
castStringToNumeric s = case s' of
    "" -> 0.0
    v@_ -> read v :: Double
    where
        s' = filterLeadingDigits s

cast :: Expr -> Type -> Interp Expr

cast (Troof v) YarnT = return $ Yarn s
    where s = case v of
            True -> "WIN"
            _ -> "FAIL"

cast (Numbr v) YarnT = return $ Yarn (show v)

cast (Numbar v) YarnT = return $ Yarn (printf "%.2f" v :: String)

cast (Yarn v) YarnT = return $ Yarn v

cast _ NoobT = return $ Noob

cast ex TroofT = do
    return $ Troof $ case ex of
        Noob -> False
        Numbr 0 -> False
        Numbar 0.0 -> False
        Yarn "" -> False
        Troof b -> b
        _ -> True

cast ex NumbrT = do
    case ex of
        Noob  -> return $ Numbr 0
        Numbr v -> return $ Numbr v
        Numbar v -> return $ Numbr $ truncate (v :: Double)
        Troof True -> return $ Numbr 1
        Troof False -> return $ Numbr 0
        Yarn s -> return $ Numbr $ truncate (castStringToNumeric s)
        p@_ -> failMessage $ "Cannot cast " ++ show p ++ " to Numbr"

cast ex NumbarT = do
    case ex of
        Noob -> return $ Numbar 0.0
        Numbr v -> return $ Numbar ((fromIntegral v) :: Double)
        Numbar v -> return $ Numbar v
        Troof True -> return $ Numbar 1.0
        Troof False -> return $ Numbar 0.0
        Yarn s -> return $ Numbar $ (castStringToNumeric s)
        p@_ -> failMessage $ "Cannot cast " ++ show p ++ " to Numbar"

cast p@_ q@_ = failMessage $ "Cannot cast " ++ (show p) ++ " to type " ++ (show q)

eval :: Expr -> Interp Expr

eval Noob = return Noob

eval (Numbr v) = return (Numbr v)

eval (Numbar v) = return (Numbar v)

eval (Yarn v) = return (Yarn v)

eval (Troof v) = return (Troof v)

eval (Var name) = lookupEnv locals name

eval (Cast p@_ vtype) = do
    ex <- eval p
    cast ex vtype

eval p@(Function name args body) = return p

eval (Call name exprs) = do
    func <- lookupEnv globals name
    case func of
        Function _ args prog -> do
            env <- get
            exprs' <- mapM eval exprs
            let locals' = zip args exprs' ++ [("IT", Noob)]
            put $ env { locals = locals'
                      , return_id = return_id env + 1
                      , return_token = 0
                      , breakable = False
                      }
            if return_id env > maxCallDepth then
                fail $ "Maximum call depth reached: " ++ (show maxCallDepth)
            else
                exec prog
            ret <- lookupEnv locals "IT"
            env' <- get
            put $ env' { locals = locals env
                       , return_id = return_id env
                       , return_token = 0
                       , breakable = breakable env
                       }
            return ret
        _ -> fail ("Attempting to call a non-function '" ++ name ++ "'")

eval (Smoosh exprs) = do
    exprs' <- mapM eval exprs
    strings <- mapM (\ex -> liftM unYarn $ eval $ Cast ex YarnT) exprs'
    return $ Yarn $ intercalate "" strings
    where unYarn x = case x of
            Yarn s -> s
            _ -> ""

eval (Not ex) = do
    Troof v <- eval (Cast ex TroofT)
    return $ Troof $ not v

eval (BinOp Sum x y) = do
    (x', y') <- getNumericPair x y
    return $ op x' y'
        where
            op (Numbr a) (Numbr b) = Numbr (a + b)
            op (Numbar a) (Numbar b) = Numbar (a + b)

eval (BinOp Diff x y) = do
    (x', y') <- getNumericPair x y
    return $ op x' y'
        where
            op (Numbr a) (Numbr b) = Numbr (a - b)
            op (Numbar a) (Numbar b) = Numbar (a - b)

eval (BinOp Produkt x y) = do
    (x', y') <- getNumericPair x y
    return $ op x' y'
        where
            op (Numbr a) (Numbr b) = Numbr (a * b)
            op (Numbar a) (Numbar b) = Numbar (a * b)

eval (BinOp Quoshunt x y) = do
    (x', y') <- getNumericPair x y
    return $ op x' y'
        where
            op (Numbr a) (Numbr b) = Numbr (a `div` b)
            op (Numbar a) (Numbar b) = Numbar (a / b)

eval (BinOp Mod x y) = do
    (x', y') <- getNumericPair x y
    op x' y'
        where
            op (Numbr a) (Numbr b) = return $ Numbr (a `mod` b)
            op (Numbar a) (Numbar b) = fail "MOD not supported for NUMBAR"

eval (BinOp Biggr x y) = do
    (x', y') <- getNumericPair x y
    return $ op x' y'
        where
            op (Numbr a) (Numbr b) = Numbr (max a b)
            op (Numbar a) (Numbar b) = Numbar (max a b)

eval (BinOp Smallr x y) = do
    (x', y') <- getNumericPair x y
    return $ op x' y'
        where
            op (Numbr a) (Numbr b) = Numbr (min a b)
            op (Numbar a) (Numbar b) = Numbar (min a b)

eval (BinOp Both x y) = evalBoolOp (&&) x y

eval (BinOp Either x y) = evalBoolOp (||) x y

eval (BinOp Won x y) = evalBoolOp (\p q -> (p || q) && (not (p && q))) x y

eval (BinOp Saem x y) = do
    x' <- eval x
    y' <- eval y
    return $ op x' y'
        where
            op (Numbr a) (Numbar b) = Troof (toDouble a == b)
            op (Numbar a) (Numbr b) = Troof (a == toDouble b)
            op p@_ q@_ = Troof (p == q)
            toDouble v = fromIntegral v :: Double

eval (BinOp Diffrint x y) = do
    Troof same <- eval (BinOp Saem x y)
    return $ Troof $ not same

eval (NaryOp All exprs) = evalBoolOpFold and exprs

eval (NaryOp Any exprs) = evalBoolOpFold or exprs

eval (DebugFail s) = fail $ "Debug fail: " ++ s

eval p@_ = fail $ "Expression not implemented: " ++ show p

evalNumeric :: Expr -> Interp Expr
evalNumeric ex = do
    ex' <- eval ex
    case ex' of
        p@(Numbr _) -> return p
        p@(Numbar _) -> return p
        p@(Troof _) -> eval (Cast p TroofT)
        p@(Yarn s) -> if '.' `elem` s
            then eval (Cast p NumbarT)
            else eval (Cast p NumbrT)
        p@_ -> fail $ "Cannot be converted to numeric: " ++ show p

getNumericPair :: Expr -> Expr -> Interp (Expr, Expr)
getNumericPair x y = do
    x' <- evalNumeric x
    y' <- evalNumeric y
    f x' y'
        where
            f p@(Numbr _) q@(Numbar _) = eval (Cast p NumbarT) >>= (\p' -> return (p', q))
            f p@(Numbar _) q@(Numbr _) = eval (Cast q NumbarT) >>= (\q' -> return (p, q'))
            f p@(Numbr _) q@_ = eval (Cast q NumbrT) >>= (\q' -> return (p, q'))
            f p@(Numbar _) q@_ = eval (Cast q NumbarT) >>= (\q' -> return (p, q'))
            f p@_ q@(Numbr _) = eval (Cast p NumbrT) >>= (\p' -> return (p', q))
            f p@_ q@(Numbar _) = eval (Cast p NumbarT) >>= (\p' -> return (p', q))
            f p@_ q@_ = return (p, q)

evalBoolOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Interp Expr
evalBoolOp f x y = do
    Troof x' <- eval (Cast x TroofT)
    Troof y' <- eval (Cast y TroofT)
    return $ Troof $ f x' y'

evalBoolOpFold :: ([Bool] -> Bool) -> [Expr] -> Interp Expr
evalBoolOpFold f exprs = do
    bools <- mapM evalAndCastBool exprs
    return $ Troof $ f bools

pushLocal :: String -> Expr -> Interp ()
pushLocal name ex = do
    env <- get
    put $ env { locals = (name, ex):(locals env) }

pushGlobal :: String -> Expr -> Interp ()
pushGlobal name ex = do
    env <- get
    put $ env { globals = (name, ex):(globals env) }

popLocal :: String -> Interp ()
popLocal name = do
    env <- get
    put $ env { locals = filter (\x -> (fst x) /= name) (locals env) }

evalAndCastBool :: Expr -> Interp (Bool)
evalAndCastBool ex = do
    ex' <- eval ex
    troof <- eval (Cast ex' TroofT)
    return $ unTroof troof
    where
        unTroof x = case x of
            Troof True -> True
            _ -> False

selectOptions :: [(Expr, Stmt)] -> Interp (Maybe Stmt)
selectOptions (x:xs) = do
    b <- evalAndCastBool $ fst x
    if b then
        return $ Just (snd x)
    else
        selectOptions xs
selectOptions [] = return Nothing

exec :: Stmt -> Interp ()

exec (Seq []) = return ()

exec (Seq (s:ss)) = do
    exec s
    env <- get
    if (return_token env) == (return_id env) then
        return ()
    else
        if (break_token env) == (break_id env) then
            return ()
        else
            exec (Seq ss)

exec (Tagged st ctx) = do
    env <- get
    put $ env { source_context = ctx }
    exec st

exec (Assign name ex) = do
    ex' <- eval ex
    pushLocal name ex'

exec (Declare name ex) = do
    ex' <- eval ex
    pushLocal name ex'

exec (ExprStmt ex) = do
    ex' <- eval ex
    pushLocal "IT" ex'
    case ex of
        p@(Function name args code) -> do
            env <- get
            if return_id env == initReturnId then
                pushGlobal name p
            else
                return () -- ignore non-global functions
        _ -> return ()

exec (Return ex) = do
    ex' <- eval ex
    pushLocal "IT" ex'
    env <- get
    put $ env { return_token = return_id env }

exec Break = do
    env <- get
    if breakable env then do
        put $ env { break_token = break_id env }
    else do
        pushLocal "IT" Noob
        env <- get
        put $ env { return_token = return_id env }

exec (Print exprs newline) = do
    Yarn s <- eval $ Smoosh exprs
    liftIO $ putStr $ s
    case newline of
        True -> liftIO $ putStr "\n"
        _ -> return ()

exec (Cast2 name tp) = do
    ex <- lookupEnv locals name >>= \x -> eval $ Cast x tp
    pushLocal name ex

exec (If yes pairs no) = do
    ex <- lookupEnv locals "IT"
    let exprs = ex : map fst pairs
        stmts = yes : map snd pairs
    pair <- selectOptions $ zip exprs stmts
    case pair of
        Just s -> exec s
        Nothing -> case no of
            Just p -> exec p
            Nothing -> return ()

exec (Case pairs defc) = do
    ref <- lookupEnv locals "IT"
    let exprs = map fst pairs
        progs = map snd pairs
        parts = map (\i -> Seq $ drop i progs) [0..(length progs - 1)]
    pair <- selectOptions $ zip (map (\x -> BinOp Saem x ref) exprs) parts
    case pair of
        Just s -> runCase s
        Nothing -> case defc of
            Just p -> runCase p
            Nothing -> return ()
    where
        runCase :: Stmt -> Interp ()
        runCase s = do
            env <- get
            enterCase env
            exec s
            exitCase env
        enterCase :: Env -> Interp ()
        enterCase env = do
            put $ env { break_id = (break_id env) + 1
                      , break_token = 0
                      , breakable = True }
        exitCase :: Env -> Interp ()
        exitCase env = do
            env' <- get
            put env' { locals = (locals env') ++ (locals env)
                     , break_id = break_id env
                     , break_token = 0
                     , breakable = breakable env }

exec (Loop _ lop lcond s) = do
    case lop of
        Increment name -> pushLocal name (Numbr 0)
        Decrement name -> pushLocal name (Numbr 0)
        UFunc _ name -> pushLocal name (Numbr 0)
        _ -> return ()
    env <- get
    put $ env { break_id = (break_id env) + 1
              , break_token = 0
              , breakable = True }
    runLoop env
    where
        runLoop :: Env -> Interp ()
        runLoop initEnv = do
            continue <- toContinue
            if continue then do
                exec s
                env <- get
                if ((return_token env) /= (return_id env) &&
                    (break_token env) /= (break_id env)) then do
                    stepLoop
                    runLoop initEnv
                else
                    exitLoop initEnv
            else
                exitLoop initEnv

        exitLoop :: Env -> Interp ()
        exitLoop env = do
            case lop of
                Increment name -> popLocal name
                Decrement name -> popLocal name
                UFunc _ name -> popLocal name
                _ -> return ()
            env' <- get
            put env' { locals = (locals env') ++ (locals env)
                     , break_id = break_id env
                     , break_token = 0
                     , breakable = breakable env }

        toContinue :: Interp Bool
        toContinue = case lcond of
            Until ex -> fmap not (evalAndCastBool ex)
            While ex -> evalAndCastBool ex
            Forever -> return True

        stepLoop :: Interp ()
        stepLoop = case lop of
            Increment name -> do
                current <- lookupEnv locals name
                val <- eval $ BinOp Sum current (Numbr 1)
                exec $ Assign name val
            Decrement name -> do
                current <- lookupEnv locals name
                val <- eval $ BinOp Diff current (Numbr 1)
                exec $ Assign name val
            UFunc func name -> do
                current <- lookupEnv locals name
                val <- eval $ Call func [current]
                exec $ Assign name val
            _ -> return ()

exec p@_ = fail $ "Statement not implemented: " ++ show p

initGlobals :: Stmt -> Store
initGlobals prog = []

emptyEnv :: Env
emptyEnv = Env { globals = []
               , locals = []
               , return_id = initReturnId
               , return_token = 0
               , break_id = initBreakId
               , break_token = 0
               , breakable = False
               , source_context = EmptyContext
               }

initEnv :: Stmt -> Env
initEnv prog = emptyEnv { globals = initGlobals prog
                        , locals = []
                        }

cleanup :: Store -> Store
cleanup store = nubBy (\a b -> fst a == fst b) store

runOnEnv :: Stmt -> Env -> IO (Env)
runOnEnv prog env = do
    (_, env') <- runStateT (exec prog) env
    let out = env' { globals = cleanup $ globals env'
                   , locals = cleanup $ locals env'
                   }
    return out

run :: Stmt -> IO (Env)
run prog = runOnEnv prog (initEnv prog)
