module Language.LOLCODE.Interpreter where

import           Control.Monad.State     (StateT, get, liftIO, liftM, modify,
                                          put, runStateT)
import           Data.List               (find, intercalate, nubBy)
import           Data.Maybe              (mapMaybe)
import           Language.LOLCODE.Syntax

type Store = [(String, Expr)]

data Env = Env { globals      :: Store
               , locals       :: Store
               , return_id    :: Int
               , return_token :: Int
               } deriving (Eq, Ord, Show)

type Interp a = StateT Env IO a

lookupEnv :: (Env -> Store) -> String -> Interp Expr
lookupEnv f name = do
    env <- get
    case lookup name (f env) of
        Nothing -> fail ("Unbounded variable '" ++ name ++ "'")
        Just ex -> return ex

eval :: Expr -> Interp Expr

eval Noob = return Noob

eval (Numbr v) = return (Numbr v)

eval (Numbar v) = return (Numbar v)

eval (Yarn v) = return (Yarn v)

eval (Troof v) = return (Troof v)

eval (Var name) = lookupEnv locals name

eval (Cast Noob YarnT) = return $ Yarn "Noob"

eval (Cast (Troof v) YarnT) = return $ Yarn s
    where s = case v of
            True -> "WIN"
            _ -> "FAIL"

eval (Cast (Numbr v) YarnT) = return $ Yarn (show v)

eval (Cast (Numbar v) YarnT) = return $ Yarn (show v)

eval (Cast (Yarn v) YarnT) = return $ Yarn v

eval (Cast _ NoobT) = return $ Noob

eval (Cast ex TroofT) = return $ Troof $ case ex of
    Numbr 0 -> False
    Numbar 0.0 -> False
    Yarn "" -> False
    Troof b -> b
    _ -> True

eval p@(Function name args body) = return p

eval (Call name exprs) = do
    func <- lookupEnv globals name
    case func of
        Function _ args prog -> do
            env <- get
            let current_locals = locals env
                current_id = return_id env
                locals' = zip args exprs ++ [("IT", Noob)]
                return_id' = current_id + 1
            put $ env { locals = locals'
                      , return_id = return_id'
                      , return_token = 0
                      }
            exec prog
            ret <- lookupEnv locals "IT"
            put $ env { locals = current_locals
                      , return_id = current_id
                      , return_token = 0
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

eval p@_ = fail $ "Expression not implemented: " ++ show p

exec :: Stmt -> Interp ()

pushLocal :: String -> Expr -> Interp ()
pushLocal name ex = do
    env <- get
    put $ env { locals = (name, ex):(locals env) }

exec (Seq []) = return ()

exec (Seq (s:ss)) = do
    exec s
    env <- get
    if (return_token env) == (return_id env) then
        return ()
    else
        exec (Seq ss)

exec (Assign name ex) = do
    ex' <- eval ex
    pushLocal name ex'

exec (Declare name ex) = do
    ex' <- eval ex
    pushLocal name ex'

exec (ExprStmt ex) = do
    ex' <- eval ex
    pushLocal "IT" ex'

exec (Return ex) = do
    ex' <- eval ex
    pushLocal "IT" ex'
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
    conds <- mapM (\p -> eval p >>= (\x -> eval (Cast x TroofT))) exprs
    let pair = find (unTroof . fst) $ zip conds stmts
            where
                unTroof x = case x of
                    Troof True -> True
                    _ -> False
    case pair of
        Just (_, s) -> exec s
        Nothing -> case no of
            Just p -> exec p
            Nothing -> return ()

exec p@_ = fail $ "Statement not implemented: " ++ show p

globalFunctions :: Stmt -> Store
globalFunctions prog = case prog of
    Seq statements -> mapMaybe justFunction statements
        where
            justFunction s = case s of
                ExprStmt ex -> case ex of
                    Function name args code -> Just (name, Function name args code)
                    _ -> Nothing
                _ -> Nothing
    ExprStmt ex -> globalFunctions $ Seq [ExprStmt ex]
    _ -> []

initGlobals :: Stmt -> Store
initGlobals prog = globalFunctions prog

emptyEnv :: Env
emptyEnv = Env { globals = []
               , locals = []
               , return_id = 1
               , return_token = 0
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
