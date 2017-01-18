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

testMaekTroof :: Assertion
testMaekTroof = checkExpr "MAEK VAR A TROOF" [Maek (Var "VAR") TroofT]

testMaekNumbr :: Assertion
testMaekNumbr = checkExpr "MAEK VAR A NUMBR" [Maek (Var "VAR") NumbrT]

testMaekNumbar :: Assertion
testMaekNumbar = checkExpr "MAEK VAR A NUMBAR" [Maek (Var "VAR") NumbarT]

testMaekYarn :: Assertion
testMaekYarn = checkExpr "MAEK VAR A YARN" [Maek (Var "VAR") YarnT]

testMaekNoob :: Assertion
testMaekNoob = checkExpr "MAEK VAR A NOOB" [Maek (Var "VAR") NoobT]

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

testCall1 :: Assertion
testCall1 = checkExpr code expected
    where
        code = [r|
        I IZ attack YR "doge" AN YR shiba MKAY
        |]
        expected = [Call "attack" [Yarn "doge", Var "shiba"]]

testCall2 :: Assertion
testCall2 = checkExpr code expected
    where
        code = [r|
        I IZ attack MKAY
        |]
        expected = [Call "attack" []]

testSmoosh1 :: Assertion
testSmoosh1 = checkExpr code expected
    where
        code = [r|
        SMOOSH "CAT" AN "SHIBA" MKAY
        |]
        expected = [Smoosh [Yarn "CAT", Yarn "SHIBA"]]

testSmoosh2 :: Assertion
testSmoosh2 = checkExpr code expected
    where
        code = [r|
        SMOOSH "CAT" AN "SHIBA"
        |]
        expected = [Smoosh [Yarn "CAT", Yarn "SHIBA"]]

testSmoosh3 :: Assertion
testSmoosh3 = checkExpr code expected
    where
        code = [r|
        SMOOSH "CAT" "SHIBA"
        |]
        expected = [Smoosh [Yarn "CAT", Yarn "SHIBA"]]

testAssign :: Assertion
testAssign = checkStmt "a R 1" (Assign "a" (Numbr 1))

testDeclare1 :: Assertion
testDeclare1 = checkStmt "I HAS A VAR" (Declare "VAR" Noob)

testDeclare2 :: Assertion
testDeclare2 = checkStmt "I HAS A VAR ITZ 1" (Declare "VAR" (Numbr 1))

testCastStmt1 :: Assertion
testCastStmt1 = checkStmt "VAR IS NOW A NOOB" (Maek2 "VAR" NoobT)

testCastStmt2A :: Assertion
testCastStmt2A = checkStmt "VAR R MAEK A NOOB" (Maek2 "VAR" NoobT)

testCastStmt2B :: Assertion
testCastStmt2B = checkStmt "VAR R MAEK NOOB" (Maek2 "VAR" NoobT)

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

testReturn :: Assertion
testReturn = checkStmt "FOUND YR shiba" (Return (Var "shiba"))

testComment1 :: Assertion
testComment1 = checkStmt "a BTW STAY AWAY FROM SHIBA KBYE" (ExprStmt (Var "a"))

testComment2 :: Assertion
testComment2 = checkStmt code expected
    where
        code = [r|
        OBTW
            THIS IS JUST A TEST
        TLDR
        a BTW THIS IS OK -- BOB'S FREN
        |]
        expected = (ExprStmt (Var "a"))

testCommaLineBreak :: Assertion
testCommaLineBreak = checkStmt code expected
    where
        code = [r|
        OBTW
            THIS IS JUST A TEST
        TLDR
        a, b BTW THIS IZ TWO LINES
        c,
        |]
        expected = Seq [(ExprStmt (Var "a")), (ExprStmt (Var "b")), (ExprStmt (Var "c"))]

tests :: TestTree
tests = testGroup "Parser"
    [ testCase "testNumbr" testNumbr
    , testCase "testNumbar" testNumbar
    , testCase "testYarn" testYarn
    , testCase "testVar" testVar
    , testCase "testMaekTroof" testMaekTroof
    , testCase "testMaekNumbr" testMaekNumbr
    , testCase "testMaekNumbar" testMaekNumbar
    , testCase "testMaekYarn" testMaekYarn
    , testCase "testMaekNoob" testMaekNoob
    , testCase "testFunction" testFunction
    , testCase "testFunction2" testFunction2
    , testCase "testCall1" testCall1
    , testCase "testCall2" testCall2
    , testCase "testSmoosh1" testSmoosh1
    , testCase "testSmoosh2" testSmoosh2
    , testCase "testSmoosh3" testSmoosh3
    , testCase "testAssign" testAssign
    , testCase "testDeclare1" testDeclare1
    , testCase "testDeclare2" testDeclare2
    , testCase "testCastStmt1" testCastStmt1
    , testCase "testCastStmt2A" testCastStmt2A
    , testCase "testCastStmt2B" testCastStmt2B
    , testCase "testIf" testIf
    , testCase "testIf2" testIf2
    , testCase "testExprStmt" testExprStmt
    , testCase "testPrintStmt" testPrintStmt
    , testCase "testPrintStmt2" testPrintStmt2
    , testCase "testReturn" testReturn
    , testCase "testComment1" testComment1
    , testCase "testComment2" testComment2
    , testCase "testCommaLineBreak" testCommaLineBreak
    ]
