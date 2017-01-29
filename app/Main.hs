module Main where

import           Language.LOLCODE.Interpreter
import           Language.LOLCODE.Parser

import           Control.Monad.Trans
import           System.Console.Haskeline
import           System.Environment
import           System.IO

process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> mapM_ print ex

runInteractive :: IO ()
runInteractive = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop

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
    [] -> runInteractive
    [filename] -> parseFile filename
    _ -> putStrLn "Usage: hcli [filename]"
