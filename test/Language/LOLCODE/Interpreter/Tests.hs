{-# LANGUAGE QuasiQuotes #-}

module Language.LOLCODE.Interpreter.Tests where

import           Data.Maybe                   (fromJust)
import           Language.LOLCODE.Interpreter
import           Language.LOLCODE.Parser
import           Language.LOLCODE.Syntax
import           Test.HUnit                   (Assertion, assertFailure, (@?=))
import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.HUnit             (testCase)
import           Text.RawString.QQ

checkRun :: String -> Env -> Assertion
checkRun code expected = case parseToplevelStmt code of
    Left err -> assertFailure $ show err
    Right prog -> do
        env <- run prog
        env @?= expected

testInitEnv :: Assertion
testInitEnv = checkRun code expected
    where
        code = [r|
        HOW IZ I attack
            SWORD R 100
        IF U SAY SO

        HOW IZ I defend
            ARMOR R 100
        IF U SAY SO

        I HAS A SHIBA ITZ 1
        SHIBA
        |]
        expected = Env { globals = [ ("attack", Function "attack" [] (Assign "SWORD" (Numbr 100)))
                                   , ("defend", Function "defend" [] (Assign "ARMOR" (Numbr 100)))
                                   ]
                       , locals = [("IT", Numbr 1), ("SHIBA", Numbr 1)]
                       }

tests :: TestTree
tests = testGroup "Interpreter"
    [ testCase "testInitEnv" testInitEnv
    ]
