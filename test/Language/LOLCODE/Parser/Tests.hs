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

testCastTroof :: Assertion
testCastTroof = checkExpr "MAEK VAR A TROOF" [Cast (Var "VAR") TroofT]

testCastNumbr :: Assertion
testCastNumbr = checkExpr "MAEK VAR A NUMBR" [Cast (Var "VAR") NumbrT]

testCastNumbar :: Assertion
testCastNumbar = checkExpr "MAEK VAR A NUMBAR" [Cast (Var "VAR") NumbarT]

testCastYarn :: Assertion
testCastYarn = checkExpr "MAEK VAR A YARN" [Cast (Var "VAR") YarnT]

testCastNoob :: Assertion
testCastNoob = checkExpr "MAEK VAR A NOOB" [Cast (Var "VAR") NoobT]

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

testSmoosh4 :: Assertion
testSmoosh4 = checkStmt code expected
    where
        code = [r|
        SMOOSH "CAT" "SHIBA"
        123
        |]
        expected = Seq [ExprStmt $ Smoosh [Yarn "CAT", Yarn "SHIBA"], ExprStmt $ Numbr 123]

testBinOp1 :: Assertion
testBinOp1 = checkExpr code expected
    where
        code = [r|
        SUM OF x AN y       BTW +
        DIFF OF x AN y      BTW -
        PRODUKT OF x AN y   BTW *
        QUOSHUNT OF x AN y  BTW /
        MOD OF x AN y       BTW modulo
        BIGGR OF x AN y     BTW max
        SMALLR OF x AN y    BTW min
        |]
        expected = [ BinOp Sum (Var "x") (Var "y")
                   , BinOp Diff (Var "x") (Var "y")
                   , BinOp Produkt (Var "x") (Var "y")
                   , BinOp Quoshunt (Var "x") (Var "y")
                   , BinOp Mod (Var "x") (Var "y")
                   , BinOp Biggr (Var "x") (Var "y")
                   , BinOp Smallr (Var "x") (Var "y")
                   ]

testBinOp2 :: Assertion
testBinOp2 = checkExpr code expected
    where
        code = [r|
        BOTH OF x AN y          BTW and: WIN iff x=WIN, y=WIN
        EITHER OF x AN y        BTW or: FAIL iff x=FAIL, y=FAIL
        WON OF x AN y           BTW xor: FAIL if x=y
        |]
        expected = [ BinOp Both (Var "x") (Var "y")
                   , BinOp Either (Var "x") (Var "y")
                   , BinOp Won (Var "x") (Var "y")
                   ]

testBinOp3 :: Assertion
testBinOp3 = checkExpr code expected
    where
        code = [r|
        BOTH SAEM x AN y   BTW WIN iff x == y
        DIFFRINT x AN y    BTW WIN iff x != y
        |]
        expected = [ BinOp Saem (Var "x") (Var "y")
                   , BinOp Diffrint (Var "x") (Var "y")
                   ]

testNaryOp :: Assertion
testNaryOp = checkExpr code expected
    where
        code = [r|
        ALL OF x AN y AN z MKAY  BTW infinite arity AND
        ANY OF x y z MKAY  BTW infinite arity OR
        |]
        expected = [ NaryOp All [(Var "x"), (Var "y"), (Var "z")]
                   , NaryOp Any [(Var "x"), (Var "y"), (Var "z")]
                   ]

testComposedOp :: Assertion
testComposedOp = checkExpr code expected
    where
        code = [r|
        BOTH SAEM x AN BIGGR OF x AN y   BTW x >= y
        BOTH SAEM x AN SMALLR OF x AN y  BTW x <= y
        DIFFRINT x AN SMALLR OF x AN y   BTW x > y
        DIFFRINT x AN BIGGR OF x AN y    BTW x < y
        |]
        expected = [ BinOp Saem (Var "x") (BinOp Biggr (Var "x") (Var "y"))
                   , BinOp Saem (Var "x") (BinOp Smallr (Var "x") (Var "y"))
                   , BinOp Diffrint (Var "x") (BinOp Smallr (Var "x") (Var "y"))
                   , BinOp Diffrint (Var "x") (BinOp Biggr (Var "x") (Var "y"))
                   ]

testNot :: Assertion
testNot = checkExpr code expected
    where
        code = [r|
        NOT x    BTW unary negation: WIN if x=FAIL
        |]
        expected = [Not (Var "x")]

testAssign :: Assertion
testAssign = checkStmt "a R 1" (Assign "a" (Numbr 1))

testDeclare1 :: Assertion
testDeclare1 = checkStmt "I HAS A VAR" (Declare "VAR" Noob)

testDeclare2 :: Assertion
testDeclare2 = checkStmt "I HAS A VAR ITZ 1" (Declare "VAR" (Numbr 1))

testCastStmt1 :: Assertion
testCastStmt1 = checkStmt "VAR IS NOW A NOOB" (Cast2 "VAR" NoobT)

testCastStmt2A :: Assertion
testCastStmt2A = checkStmt "VAR R MAEK A NOOB" (Cast2 "VAR" NoobT)

testCastStmt2B :: Assertion
testCastStmt2B = checkStmt "VAR R MAEK NOOB" (Cast2 "VAR" NoobT)

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
        expected = If (Assign "a" (Numbr 1)) [] (Just (Assign "a" (Numbr 2)))

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
        expected = If (Assign "a" (Numbr 1)) [(Var "b", Assign "a" (Numbr 10)), (Var "c", Assign "a" (Numbr 11))] (Just (Assign "a" (Numbr 2)))

testIf3 :: Assertion
testIf3 = checkStmt code expected
    where
        code = [r|
        O RLY?
            YA RLY
                a R 1
        OIC
        |]
        expected = If (Assign "a" (Numbr 1)) [] Nothing

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

testLoop1 :: Assertion
testLoop1 = checkStmt code expected
    where
        code = [r|
        IM IN YR doge UPPIN YR cat TIL BOTH SAEM cat 0
            dog R "SHIBA"
        IM OUTTA YR doge
        |]
        expected = Loop "doge" (Increment "cat") (Until (BinOp Saem (Var "cat") (Numbr 0))) (Assign "dog" (Yarn "SHIBA"))

testLoop2 :: Assertion
testLoop2 = checkStmt code expected
    where
        code = [r|
        IM IN YR doge NERFIN YR cat TIL BOTH SAEM cat 0
            dog R "SHIBA"
        IM OUTTA YR doge
        |]
        expected = Loop "doge" (Decrement "cat") (Until (BinOp Saem (Var "cat") (Numbr 0))) (Assign "dog" (Yarn "SHIBA"))

testLoop3 :: Assertion
testLoop3 = checkStmt code expected
    where
        code = [r|
        IM IN YR doge UPPIN YR cat WILE BOTH SAEM cat 0
            dog R "SHIBA"
        IM OUTTA YR doge
        |]
        expected = Loop "doge" (Increment "cat") (While (BinOp Saem (Var "cat") (Numbr 0))) (Assign "dog" (Yarn "SHIBA"))

testLoop4 :: Assertion
testLoop4 = checkStmt code expected
    where
        code = [r|
        IM IN YR doge petting YR cat WILE BOTH SAEM cat 0
            dog R "SHIBA"
        IM OUTTA YR doge
        |]
        expected = Loop "doge" (UFunc "petting" "cat") (While (BinOp Saem (Var "cat") (Numbr 0))) (Assign "dog" (Yarn "SHIBA"))

testLoop5 :: Assertion
testLoop5 = checkStmt code expected
    where
        code = [r|
        IM IN YR doge petting YR cat
            dog R "SHIBA"
            GTFO
        IM OUTTA YR doge
        |]
        expected = Loop "doge" (UFunc "petting" "cat") Forever (Seq [Assign "dog" (Yarn "SHIBA"), Break])

testLoop6 :: Assertion
testLoop6 = checkStmt code expected
    where
        code = [r|
        IM IN YR doge
            dog R "SHIBA"
            GTFO
        IM OUTTA YR doge
        |]
        expected = Loop "doge" Noop Forever (Seq [Assign "dog" (Yarn "SHIBA"), Break])

testSwitch1 :: Assertion
testSwitch1 = checkStmt code expected
    where
        code = [r|
        WTF?
          OMG "R"
            1
          OMG "B"
            2
          OMGWTF
            3
        OIC
        |]
        expected = Case [(Yarn "R", ExprStmt (Numbr 1)), (Yarn "B", ExprStmt (Numbr 2))] (Just (ExprStmt (Numbr 3)))

testSwitch2 :: Assertion
testSwitch2 = checkStmt code expected
    where
        code = [r|
        WTF?
          OMG "R"
            1
          OMG "B"
          OMGWTF
            3
        OIC
        |]
        expected = Case [(Yarn "R", ExprStmt (Numbr 1)), (Yarn "B", Seq [])] (Just (ExprStmt (Numbr 3)))

testParens :: Assertion
testParens = checkStmt code expected
    where
        code = [r|
        ((VISIBLE "SHIBA" "INU"))
        (123)
        456
        |]
        expected = Seq [s1, s2, s3]
        s1 = Print [(Yarn "SHIBA"), (Yarn "INU")] True
        s2 = ExprStmt (Numbr 123)
        s3 = ExprStmt (Numbr 456)

testVisible :: Assertion
testVisible = checkStmt code expected
    where
        code = [r|
        VISIBLE "SHIBA" "INU"
        123
        |]
        expected = Seq [s1, s2]
        s1 = Print [(Yarn "SHIBA"), (Yarn "INU")] True
        s2 = ExprStmt (Numbr 123)

tests :: TestTree
tests = testGroup "Parser"
    [ testCase "testNumbr" testNumbr
    , testCase "testNumbar" testNumbar
    , testCase "testYarn" testYarn
    , testCase "testVar" testVar
    , testCase "testCastTroof" testCastTroof
    , testCase "testCastNumbr" testCastNumbr
    , testCase "testCastNumbar" testCastNumbar
    , testCase "testCastYarn" testCastYarn
    , testCase "testCastNoob" testCastNoob
    , testCase "testFunction" testFunction
    , testCase "testFunction2" testFunction2
    , testCase "testCall1" testCall1
    , testCase "testCall2" testCall2
    , testCase "testSmoosh1" testSmoosh1
    , testCase "testSmoosh2" testSmoosh2
    , testCase "testSmoosh3" testSmoosh3
    , testCase "testSmoosh4" testSmoosh4
    , testCase "testBinOp1" testBinOp1
    , testCase "testBinOp2" testBinOp2
    , testCase "testBinOp3" testBinOp3
    , testCase "testComposedOp" testComposedOp
    , testCase "testNot" testNot
    , testCase "testNaryOp" testNaryOp
    , testCase "testAssign" testAssign
    , testCase "testDeclare1" testDeclare1
    , testCase "testDeclare2" testDeclare2
    , testCase "testCastStmt1" testCastStmt1
    , testCase "testCastStmt2A" testCastStmt2A
    , testCase "testCastStmt2B" testCastStmt2B
    , testCase "testIf" testIf
    , testCase "testIf2" testIf2
    , testCase "testIf3" testIf3
    , testCase "testExprStmt" testExprStmt
    , testCase "testPrintStmt" testPrintStmt
    , testCase "testPrintStmt2" testPrintStmt2
    , testCase "testReturn" testReturn
    , testCase "testComment1" testComment1
    , testCase "testComment2" testComment2
    , testCase "testCommaLineBreak" testCommaLineBreak
    , testCase "testLoop1" testLoop1
    , testCase "testLoop2" testLoop2
    , testCase "testLoop3" testLoop3
    , testCase "testLoop4" testLoop4
    , testCase "testLoop5" testLoop5
    , testCase "testLoop6" testLoop6
    , testCase "testSwitch1" testSwitch1
    , testCase "testSwitch2" testSwitch2
    , testCase "testParens" testParens
    , testCase "testVisible" testVisible
    ]
