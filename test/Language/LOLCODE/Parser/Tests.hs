{-# LANGUAGE QuasiQuotes #-}

module Language.LOLCODE.Parser.Tests where

import           Data.Maybe              (fromJust)
import           Language.LOLCODE.Parser
import           Language.LOLCODE.Syntax
import           Test.HUnit              (Assertion, assertFailure, (@?=))
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase)
import           Text.RawString.QQ

checkExpr :: String -> [Expr] -> Assertion
checkExpr code expected = case parseToplevel code of
    Left err -> assertFailure $ show err
    Right ex -> ex @?= expected

checkStmt :: String -> Stmt -> Assertion
checkStmt code expected = case parseToplevelStmt code of
    Left err -> assertFailure $ show err
    Right st -> st @?= expected

testNumbr :: Assertion
testNumbr = checkExpr "1" [Numbr 1]

testNumbar :: Assertion
testNumbar = checkExpr "1.0" [Numbar 1.0]

testYarn :: Assertion
testYarn = checkExpr "\"abc def\"" [Yarn "abc def"]

testVar :: Assertion
testVar = checkExpr "a" [Var "a"]

testFunction :: Assertion
testFunction = checkExpr code expected
    where
        code = [r|
        HOW IZ I attack
            life R 0
        IF U SAY SO
        |]
        expected = [Function "attack" [] (Assign "life" (Numbr 0))]

testFunction2 :: Assertion
testFunction2 = checkExpr code expected
    where
        code = [r|
        HOW IZ I attack YR target AN YR power
            life R 0
        IF U SAY SO
        |]
        expected = [Function "attack" ["target", "power"] (Assign "life" (Numbr 0))]

testAssign :: Assertion
testAssign = checkStmt "a R 1" (Assign "a" (Numbr 1))

testIf :: Assertion
testIf = checkStmt code expected
    where
        code = [r|
        O RLY?
            YA RLY
                a R 1
            NO WAI
                a R 2
        OIC
        |]
        expected = If (Assign "a" (Numbr 1)) [] (Assign "a" (Numbr 2))

testIf2 :: Assertion
testIf2 = checkStmt code expected
    where
        code = [r|
        O RLY?
            YA RLY
                a R 1
            MEBBE b
                a R 10
            MEBBE c
                a R 11
            NO WAI
                a R 2
        OIC
        |]
        expected = If (Assign "a" (Numbr 1)) [(Var "b", Assign "a" (Numbr 10)), (Var "c", Assign "a" (Numbr 11))] (Assign "a" (Numbr 2))

testExprStmt :: Assertion
testExprStmt = checkStmt "1" (ExprStmt (Numbr 1))

testPrintStmt :: Assertion
testPrintStmt = checkStmt code expected
    where
        code = [r|
        VISIBLE "a" 1 1.0
        |]
        expected = Print [Yarn "a", Numbr 1, Numbar 1.0] True

testPrintStmt2 :: Assertion
testPrintStmt2 = checkStmt code expected
    where
        code = [r|
        VISIBLE "a" 1 1.0!
        |]
        expected = Print [Yarn "a", Numbr 1, Numbar 1.0] False

tests :: TestTree
tests = testGroup "Parser"
    [ testCase "testNumbr" testNumbr
    , testCase "testNumbar" testNumbar
    , testCase "testYarn" testYarn
    , testCase "testVar" testVar
    , testCase "testFunction" testFunction
    , testCase "testFunction2" testFunction2
    , testCase "testAssign" testAssign
    , testCase "testIf" testIf
    , testCase "testIf2" testIf2
    , testCase "testExprStmt" testExprStmt
    , testCase "testPrintStmt" testPrintStmt
    , testCase "testPrintStmt2" testPrintStmt2
    ]
