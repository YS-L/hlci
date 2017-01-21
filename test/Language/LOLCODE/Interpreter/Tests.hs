{-# LANGUAGE QuasiQuotes #-}

module Language.LOLCODE.Interpreter.Tests where

import           Control.Monad                (forM_)
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

checkStore :: (Env -> Store) -> String -> [(String, Expr)] -> Assertion
checkStore f code expected = case parseToplevelStmt code of
    Left err -> assertFailure $ show err
    Right prog -> do
        env <- run prog
        forM_ expected (\p -> case lookup (fst p) (f env) of
            Just x -> x @?= (snd p)
            Nothing -> assertFailure $ "Expected key not found: " ++ (fst p))

checkStoreLocal :: String -> [(String, Expr)] -> Assertion
checkStoreLocal = checkStore locals

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

testCall :: Assertion
testCall = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I attack YR value
            SWORD R value
            SWORD
        IF U SAY SO

        I HAS A SWORD ITZ (I IZ attack YR 123 MKAY)
        SWORD
        |]
        expected = [("IT", Numbr 123), ("SWORD", Numbr 123)]

testReturn :: Assertion
testReturn = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I gimmeh_sword
            FOUND YR "stick"
            FOUND YR "SHOULD NOT BE HERE!!1"
        IF U SAY SO

        I HAS A SWORD ITZ (I IZ gimmeh_sword MKAY)
        SWORD
        |]
        expected = [("IT", Yarn "stick"), ("SWORD", Yarn "stick")]

testPrint :: Assertion
testPrint = checkStoreLocal code []
    where
        code = [r|
        I HAS A CAT ITZ 123
        I HAS A SHIBA ITZ "abc"
        I HAS A DOGE ITZ 1.23
        I HAS A WOW ITZ WIN
        VISIBLE CAT SHIBA DOGE WOW " "!
        VISIBLE CAT SHIBA DOGE WOW
        |]

testCastStmt :: Assertion
testCastStmt = checkStoreLocal code expected
    where
        code = [r|
        I HAS A VAR ITZ 1
        VAR2 R VAR
        VAR IS NOW A TROOF
        |]
        expected = [("VAR", Troof True), ("VAR2", Numbr 1)]

testIf1 :: Assertion
testIf1 = checkStoreLocal code expected
    where
        code = [r|
        WIN
        O RLY?
          YA RLY
            VAR R 1
          NO WAI
            VAR R 0
        OIC
        |]
        expected = [("VAR", Numbr 1)]

testIf2 :: Assertion
testIf2 = checkStoreLocal code expected
    where
        code = [r|
        FAIL
        O RLY?
          YA RLY
            VAR R 1
          NO WAI
            VAR R 0
        OIC
        |]
        expected = [("VAR", Numbr 0)]

testIf3 :: Assertion
testIf3 = checkStoreLocal code expected
    where
        code = [r|
        FAIL
        O RLY?
          YA RLY
            VAR R 1
          MEBBE WIN
            VAR R 2
          NO WAI
            VAR R 0
        OIC
        |]
        expected = [("VAR", Numbr 2)]

testIf4 :: Assertion
testIf4 = checkStoreLocal code expected
    where
        code = [r|
        0, O RLY?
          YA RLY
            VAR R 1
          MEBBE 0.0
            VAR R 2
          NO WAI
            VAR R 100
            VAR2 R 101
        OIC
        |]
        expected = [("VAR", Numbr 100), ("VAR2", Numbr 101)]

testReturnInSubProgram :: Assertion
testReturnInSubProgram = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I gimmeh_sword
            1, O RLY?
            YA RLY
                FOUND YR "stick"
            OIC
            FOUND YR "SHOULD NOT BE HERE!!1"
        IF U SAY SO

        I HAS A SWORD ITZ (I IZ gimmeh_sword MKAY)
        SWORD
        |]
        expected = [("IT", Yarn "stick"), ("SWORD", Yarn "stick")]

tests :: TestTree
tests = testGroup "Interpreter"
    [ testCase "testInitEnv" testInitEnv
    , testCase "testCall" testCall
    , testCase "testReturn" testReturn
    , testCase "testPrint" testPrint
    , testCase "testCastStmt" testCastStmt
    , testCase "testIf1" testIf1
    , testCase "testIf2" testIf2
    , testCase "testIf3" testIf3
    , testCase "testIf4" testIf4
    , testCase "testReturnInSubProgram" testReturnInSubProgram
    ]
