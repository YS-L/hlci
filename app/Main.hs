module Main where

import           Language.LOLCODE.Interpreter
import           Language.LOLCODE.Parser

import           Control.Monad.Trans
import           System.Environment
import           System.IO

parseFile :: String -> IO ()
parseFile filename = do
    content <- readFile filename
    let parsed = parseToplevelStmtWithFilename filename content
    case parsed of
        Left err -> print err
        Right prog -> do
            run prog
            return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> parseFile filename
    _ -> putStrLn "Usage: hcli filename"
