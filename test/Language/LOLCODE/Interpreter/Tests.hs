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
        expected = emptyEnv { globals = [ ("attack", Function "attack" [] (Assign "SWORD" (Numbr 100)))
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

testReturnInSubProgram2 :: Assertion
testReturnInSubProgram2 = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I gimmeh_sword
            1, O RLY?
            YA RLY
                FOUND YR "stick"
            OIC
            FOUND YR "SHOULD NOT BE HERE!!1"
        IF U SAY SO

        HOW IZ I gimmeh_shield
            1, O RLY?
            YA RLY
                FOUND YR "board"
            OIC
            FOUND YR "SHOULD NOT BE HERE!!1"
        IF U SAY SO

        HOW IZ I gimmeh_thingz
            0, O RLY?
            YA RLY
                FOUND YR "SHOULD NOT BE HERE!!1"
            NO WAI
                I HAS A SWORD ITZ (I IZ gimmeh_sword MKAY)
                I HAS A SHIELD ITZ (I IZ gimmeh_shield MKAY)
            OIC
            FOUND YR (SMOOSH SWORD "_" SHIELD MKAY)
        IF U SAY SO

        I HAS A THINGZ ITZ (I IZ gimmeh_thingz MKAY)
        |]
        expected = [("THINGZ", Yarn "stick_board")]

testOpSum :: Assertion
testOpSum = checkStoreLocal code expected
    where
        code = [r|
        v1 R SUM OF 1 1
        v2 R SUM OF 1 1.0
        v3 R SUM OF 1.0 1
        v4 R SUM OF "1" 1
        v5 R SUM OF 1.0 "1"
        |]
        expected = [ ("v1", Numbr 2)
                   , ("v2", Numbar 2.0)
                   , ("v3", Numbar 2.0)
                   , ("v4", Numbr 2)
                   , ("v5", Numbar 2.0)]

testOpDiff :: Assertion
testOpDiff = checkStoreLocal code expected
    where
        code = [r|
        v1 R DIFF OF 1 1
        v2 R DIFF OF 1 1.0
        v3 R DIFF OF 1.0 1
        v4 R DIFF OF "1" 1
        v5 R DIFF OF 1.0 "1"
        |]
        expected = [ ("v1", Numbr 0)
                   , ("v2", Numbar 0.0)
                   , ("v3", Numbar 0.0)
                   , ("v4", Numbr 0)
                   , ("v5", Numbar 0.0)]

testOpProd :: Assertion
testOpProd = checkStoreLocal code expected
    where
        code = [r|
        v1 R PRODUKT OF 1 1
        v2 R PRODUKT OF 1 1.0
        v3 R PRODUKT OF 1.0 1
        v4 R PRODUKT OF "1" 1
        v5 R PRODUKT OF 1.0 "1"
        |]
        expected = [ ("v1", Numbr 1)
                   , ("v2", Numbar 1.0)
                   , ("v3", Numbar 1.0)
                   , ("v4", Numbr 1)
                   , ("v5", Numbar 1.0)]

testOpQuoshunt :: Assertion
testOpQuoshunt = checkStoreLocal code expected
    where
        code = [r|
        v1 R QUOSHUNT OF 5 2
        v2 R QUOSHUNT OF 5 2.0
        v3 R QUOSHUNT OF 5.0 2
        v4 R QUOSHUNT OF "5" 2
        v5 R QUOSHUNT OF 5.0 "2"
        |]
        expected = [ ("v1", Numbr 2)
                   , ("v2", Numbar 2.5)
                   , ("v3", Numbar 2.5)
                   , ("v4", Numbr 2)
                   , ("v5", Numbar 2.5)]

testOpMod :: Assertion
testOpMod = checkStoreLocal code expected
    where
        code = [r|
        v1 R MOD OF 5 2
        v4 R MOD OF "5" 2
        |]
        expected = [ ("v1", Numbr 1)
                   , ("v4", Numbr 1)]

testOpBiggr :: Assertion
testOpBiggr = checkStoreLocal code expected
    where
        code = [r|
        v1 R BIGGR OF 5 2
        v2 R BIGGR OF 5 2.0
        v3 R BIGGR OF 5.0 2
        v4 R BIGGR OF "5" 2
        v5 R BIGGR OF 5.0 "2"
        |]
        expected = [ ("v1", Numbr 5)
                   , ("v2", Numbar 5.0)
                   , ("v3", Numbar 5.0)
                   , ("v4", Numbr 5)
                   , ("v5", Numbar 5.0)]

testOpSmallr :: Assertion
testOpSmallr = checkStoreLocal code expected
    where
        code = [r|
        v1 R SMALLR OF 5 2
        v2 R SMALLR OF 5 2.0
        v3 R SMALLR OF 5.0 2
        v4 R SMALLR OF "5" 2
        v5 R SMALLR OF 5.0 "2"
        |]
        expected = [ ("v1", Numbr 2)
                   , ("v2", Numbar 2.0)
                   , ("v3", Numbar 2.0)
                   , ("v4", Numbr 2)
                   , ("v5", Numbar 2.0)]

testOpBoth :: Assertion
testOpBoth = checkStoreLocal code expected
    where
        code = [r|
        v1 R BOTH OF WIN WIN
        v2 R BOTH OF WIN FAIL
        v3 R BOTH OF FAIL WIN
        v4 R BOTH OF FAIL FAIL
        v5 R BOTH OF 5 2.0
        |]
        expected = [ ("v1", Troof True)
                   , ("v2", Troof False)
                   , ("v3", Troof False)
                   , ("v4", Troof False)
                   , ("v5", Troof True)]

testOpEither :: Assertion
testOpEither = checkStoreLocal code expected
    where
        code = [r|
        v1 R EITHER OF WIN WIN
        v2 R EITHER OF WIN FAIL
        v3 R EITHER OF FAIL WIN
        v4 R EITHER OF FAIL FAIL
        v5 R EITHER OF 5 2.0
        |]
        expected = [ ("v1", Troof True)
                   , ("v2", Troof True)
                   , ("v3", Troof True)
                   , ("v4", Troof False)
                   , ("v5", Troof True)]

testOpWon :: Assertion
testOpWon = checkStoreLocal code expected
    where
        code = [r|
        v1 R WON OF WIN WIN
        v2 R WON OF WIN FAIL
        v3 R WON OF FAIL WIN
        v4 R WON OF FAIL FAIL
        v5 R WON OF 5 2.0
        |]
        expected = [ ("v1", Troof False)
                   , ("v2", Troof True)
                   , ("v3", Troof True)
                   , ("v4", Troof False)
                   , ("v5", Troof False)]

testOpSaem :: Assertion
testOpSaem = checkStoreLocal code expected
    where
        code = [r|
        v1 R BOTH SAEM 1 1
        v2 R BOTH SAEM 1.0 1.0
        v3 R BOTH SAEM 1 1.0
        v4 R BOTH SAEM 1 "1"
        v5 R BOTH SAEM "1" "1"
        |]
        expected = [ ("v1", Troof True)
                   , ("v2", Troof True)
                   , ("v3", Troof True)
                   , ("v4", Troof False)
                   , ("v5", Troof True)]

testOpDiffrint :: Assertion
testOpDiffrint = checkStoreLocal code expected
    where
        code = [r|
        v1 R DIFFRINT 1 1
        v2 R DIFFRINT 1.0 1.0
        v3 R DIFFRINT 1 1.0
        v4 R DIFFRINT 1 "1"
        v5 R DIFFRINT "1" "1"
        |]
        expected = [ ("v1", Troof False)
                   , ("v2", Troof False)
                   , ("v3", Troof False)
                   , ("v4", Troof True)
                   , ("v5", Troof False)]

testOpAll :: Assertion
testOpAll = checkStoreLocal code expected
    where
        code = [r|
        v1 R ALL OF 1 1 MKAY
        v2 R ALL OF 1.0 1.0 MKAY
        v3 R ALL OF 1 1.0 MKAY
        v4 R ALL OF 1 "" MKAY
        v5 R ALL OF "1" "1" MKAY
        |]
        expected = [ ("v1", Troof True)
                   , ("v2", Troof True)
                   , ("v3", Troof True)
                   , ("v4", Troof False)
                   , ("v5", Troof True)]

testOpAny :: Assertion
testOpAny = checkStoreLocal code expected
    where
        code = [r|
        v1 R ANY OF FAIL 0 MKAY
        v2 R ANY OF 1.0 1.0 MKAY
        v3 R ANY OF 1 1.0 MKAY
        v4 R ANY OF 1 "" MKAY
        v5 R ANY OF 0.0 "" MKAY
        |]
        expected = [ ("v1", Troof False)
                   , ("v2", Troof True)
                   , ("v3", Troof True)
                   , ("v4", Troof True)
                   , ("v5", Troof False)]

testOpNot :: Assertion
testOpNot = checkStoreLocal code expected
    where
        code = [r|
        v1 R NOT FAIL
        v2 R NOT WIN
        v3 R NOT "1"
        v4 R NOT 0
        v5 R NOT 1.0
        |]
        expected = [ ("v1", Troof True)
                   , ("v2", Troof False)
                   , ("v3", Troof False)
                   , ("v4", Troof True)
                   , ("v5", Troof False)]

testBreakFromFunction :: Assertion
testBreakFromFunction = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I foo
            1, O RLY?
            YA RLY
                GTFO
                FOUND YR "SHOULD NOT BE HERE!!1"
            OIC
        IF U SAY SO

        I HAS A var ITZ (I IZ foo MKAY)
        |]
        expected = [("var", Noob)]

testSwitch1 :: Assertion
testSwitch1 = checkStoreLocal code expected
    where
        code = [r|
        color R "G"
        I HAS A var

        color, WTF?
          OMG "R"
            var R "RED FISH"
            GTFO
          OMG "Y"
            var R "YELLOW FISH"
          OMG "G"
          OMG "B"
            var R "FISH HAS A FLAVOR"
            GTFO
          OMGWTF
            var R "FISH IS TRANSPARENT"
        OIC
        |]
        expected = [("var", Yarn "FISH HAS A FLAVOR")]

testSwitch2 :: Assertion
testSwitch2 = checkStoreLocal code expected
    where
        code = [r|
        color R "LOOOL"
        I HAS A var

        color, WTF?
          OMG "R"
            var R "RED FISH"
            GTFO
          OMG "Y"
            var R "YELLOW FISH"
          OMG "G"
          OMG "B"
            var R "FISH HAS A FLAVOR"
            GTFO
          OMGWTF
            var R "FISH IS TRANSPARENT"
        OIC
        |]
        expected = [("var", Yarn "FISH IS TRANSPARENT")]

testSwitch3 :: Assertion
testSwitch3 = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I check_fish

            color R "G"
            I HAS A var

            color, WTF?
              OMG "R"
                var R "RED FISH"
                GTFO
              OMG "Y"
                var R "YELLOW FISH"
              OMG "G"
              OMG "B"
                var R "FISH HAS A FLAVOR"
                GTFO
              OMGWTF
                var R "FISH IS TRANSPARENT"
            OIC

            FOUND YR var

        IF U SAY SO

        out R (I IZ check_fish MKAY)
        |]
        expected = [("out", Yarn "FISH HAS A FLAVOR")]

testSwitch4 :: Assertion
testSwitch4 = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I foo

            color R "R"

            color, WTF?
              OMG "R"
                FOUND YR "CORRECT"
              OMG "Y"
                FOUND YR "WRONG_Y"
              OMG "G"
              OMG "B"
                FOUND YR "WRONG_GB"
              OMGWTF
                FOUND YR "WRONG_WTF"
            OIC

            FOUND YR "WRONG_WTF2"

        IF U SAY SO

        out R (I IZ foo MKAY)
        |]
        expected = [("out", Yarn "CORRECT")]

testSwitch5 :: Assertion
testSwitch5 = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I foo

            color R "R"

            color, WTF?
              OMG "R"
                "WRONG_0"
                GTFO
                FOUND YR "WRONG_R"
              OMG "Y"
                FOUND YR "WRONG_Y"
              OMG "G"
              OMG "B"
                FOUND YR "WRONG_GB"
              OMGWTF
                FOUND YR "WRONG_WTF"
            OIC

            FOUND YR "CORRECT"

        IF U SAY SO

        out R (I IZ foo MKAY)
        |]
        expected = [("out", Yarn "CORRECT")]

testNestedFunction :: Assertion
testNestedFunction = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I foo
            1, O RLY?
            YA RLY
                FOUND YR (SUM OF 1 (I IZ bar MKAY))
            OIC
        IF U SAY SO

        HOW IZ I bar
            1, O RLY?
            YA RLY
                FOUND YR 1
            OIC
        IF U SAY SO

        out R (I IZ foo MKAY)
        |]
        expected = [("out", Numbr 2)]

testNestedFunctionWithCase1 :: Assertion
testNestedFunctionWithCase1 = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I foo
            1, O RLY?
            YA RLY
                "A", WTF?
                OMG "A"
                    FOUND YR (SUM OF 1 (I IZ bar MKAY))
                OIC
            OIC
        IF U SAY SO

        HOW IZ I bar
            1, O RLY?
            YA RLY
                FOUND YR 1
            OIC
        IF U SAY SO

        out R (I IZ foo MKAY)
        |]
        expected = [("out", Numbr 2)]

testNestedFunctionWithCase2 :: Assertion
testNestedFunctionWithCase2 = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I foo
            1, O RLY?
            YA RLY
                "A", WTF?
                OMG "A"
                    FOUND YR (SUM OF 1 (I IZ bar MKAY))
                OIC
            OIC
        IF U SAY SO

        HOW IZ I bar
            1, O RLY?
            YA RLY
                "B", WTF?
                OMG "A"
                    FOUND YR "WRONG"
                OMGWTF
                    FOUND YR 1
                OIC
            OIC
        IF U SAY SO

        out R (I IZ foo MKAY)
        |]
        expected = [("out", Numbr 2)]

testNestedFunctionWithCase3 :: Assertion
testNestedFunctionWithCase3 = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I foo
            "WRONG"
            GTFO
            FOUND YR food
        IF U SAY SO

        "A", WTF?
        OMG "A"
            out R (I IZ foo MKAY)
            GTFO
            out R "WRONG2"
        OIC
        |]
        expected = [("out", Noob)]

testNestedFunctionWithCase4 :: Assertion
testNestedFunctionWithCase4 = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I foo
            "WRONG"
            GTFO
            FOUND YR food
        IF U SAY SO

        HOW IZ I bar
            "A", WTF?
            OMG "A"
                v R (I IZ foo MKAY)
                GTFO
                v R "WRONG2"
            OIC
            FOUND YR v
        IF U SAY SO

        out R (I IZ bar MKAY)
        |]
        expected = [("out", Noob)]

testRecursiveFunction :: Assertion
testRecursiveFunction = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I fiboz YR n

            BOTH SAEM n 2, O RLY?
            YA RLY
                FOUND YR 1
            OIC

            BOTH SAEM n 1, O RLY?
            YA RLY
                FOUND YR 1
            OIC

            v1 R (I IZ fiboz YR (DIFF OF n 1) MKAY)
            BTW v1 R 0
            v2 R (I IZ fiboz YR (DIFF OF n 2) MKAY)
            BTW v2 R 0

            FOUND YR (SUM OF v1 v2)
        IF U SAY SO

        n3 R (I IZ fiboz YR 3 MKAY)
        n4 R (I IZ fiboz YR 4 MKAY)
        n5 R (I IZ fiboz YR 5 MKAY)
        n10 R (I IZ fiboz YR 10 MKAY)
        |]
        expected = [ ("n3", Numbr 2)
                   , ("n4", Numbr 3)
                   , ("n5", Numbr 5)
                   , ("n10", Numbr 55)
                   ]

testLoop1 :: Assertion
testLoop1 = checkStoreLocal code expected
    where
        code = [r|
        sum R 0
        IM IN YR sumz UPPIN YR n TIL BOTH SAEM n 10
            sum R SUM OF sum n
        IM OUTTA YR sumz
        |]
        expected = [("sum", Numbr 45)]

testLoop2 :: Assertion
testLoop2 = checkStoreLocal code expected
    where
        code = [r|
        sum R 0
        IM IN YR sumz NERFIN YR n TIL BOTH SAEM n -10
            sum R SUM OF sum n
        IM OUTTA YR sumz
        |]
        expected = [("sum", Numbr (-45))]

testLoop3 :: Assertion
testLoop3 = checkStoreLocal code expected
    where
        code = [r|
        sum R 0
        IM IN YR sumz UPPIN YR n TIL BOTH SAEM n 10
            sum R SUM OF sum n
            GTFO
        IM OUTTA YR sumz
        |]
        expected = [("sum", Numbr 0)]

testLoop4 :: Assertion
testLoop4 = checkStoreLocal code expected
    where
        code = [r|
        sum R 0
        IM IN YR sumz
            sum R SUM OF sum 1
            BOTH SAEM sum 55, O RLY?
            YA RLY
                GTFO
            OIC
        IM OUTTA YR sumz
        |]
        expected = [("sum", Numbr 55)]

testLoop5 :: Assertion
testLoop5 = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I myUppin YR val
            FOUND YR SUM OF val 1
        IF U SAY SO

        sum R 0
        IM IN YR sumz myUppin YR n TIL BOTH SAEM n 10
            sum R SUM OF sum n
        IM OUTTA YR sumz
        |]
        expected = [("sum", Numbr 45)]

testLoopWithinFunction :: Assertion
testLoopWithinFunction = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I foo

            sum R 0

            IM IN YR sumz
                sum R SUM OF sum 1
                BOTH SAEM sum 55, O RLY?
                YA RLY
                    GTFO
                OIC
            IM OUTTA YR sumz

            FOUND YR sum

        IF U SAY SO

        out R I IZ foo MKAY
        |]
        expected = [("out", Numbr 55)]

testLoopWithinFunction2 :: Assertion
testLoopWithinFunction2 = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I foo

            sum R 0

            IM IN YR sumz
                sum R SUM OF sum 1
                BOTH SAEM sum 55, O RLY?
                YA RLY
                    FOUND YR sum
                OIC
            IM OUTTA YR sumz

            FOUND YR "WRONG"

        IF U SAY SO

        out R I IZ foo MKAY
        |]
        expected = [("out", Numbr 55)]

testLoopWithinSwitch :: Assertion
testLoopWithinSwitch = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I foo

            sum R 0

            "A", WTF?
            OMG "A"
                IM IN YR sumz
                    sum R SUM OF sum 1
                    BOTH SAEM sum 55, O RLY?
                    YA RLY
                        GTFO
                    OIC
                IM OUTTA YR sumz
                sum R SUM OF sum 10
            OIC

            FOUND YR sum

        IF U SAY SO

        out R I IZ foo MKAY
        |]
        expected = [("out", Numbr 65)]

testLoopNested :: Assertion
testLoopNested = checkStoreLocal code expected
    where
        code = [r|
        HOW IZ I foo

            sum R 0

            IM IN YR sumz UPPIN YR n1 TIL BOTH SAEM n1 10
                IM IN YR sumz2 UPPIN YR n2 TIL BOTH SAEM n2 10
                    sum R SUM OF sum 1
                IM OUTTA YR sumz2
            IM OUTTA YR sumz

            FOUND YR sum

        IF U SAY SO

        out R I IZ foo MKAY
        |]
        expected = [("out", Numbr 100)]

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
    , testCase "testReturnInSubProgram2" testReturnInSubProgram2
    , testCase "testOpSum" testOpSum
    , testCase "testOpDiff" testOpDiff
    , testCase "testOpProd" testOpProd
    , testCase "testOpQuoshunt" testOpQuoshunt
    , testCase "testOpMod" testOpMod
    , testCase "testOpBiggr" testOpBiggr
    , testCase "testOpSmallr" testOpSmallr
    , testCase "testOpBoth" testOpBoth
    , testCase "testOpEither" testOpEither
    , testCase "testOpWon" testOpWon
    , testCase "testOpSaem" testOpSaem
    , testCase "testOpDiffrint" testOpDiffrint
    , testCase "testOpAll" testOpAll
    , testCase "testOpAny" testOpAny
    , testCase "testOpNot" testOpNot
    , testCase "testBreakFromFunction" testBreakFromFunction
    , testCase "testSwitch1" testSwitch1
    , testCase "testSwitch2" testSwitch2
    , testCase "testSwitch3" testSwitch3
    , testCase "testSwitch4" testSwitch4
    , testCase "testSwitch5" testSwitch5
    , testCase "testNestedFunction" testNestedFunction
    , testCase "testNestedFunctionWithCase1" testNestedFunctionWithCase1
    , testCase "testNestedFunctionWithCase2" testNestedFunctionWithCase2
    , testCase "testNestedFunctionWithCase3" testNestedFunctionWithCase3
    , testCase "testNestedFunctionWithCase4" testNestedFunctionWithCase4
    , testCase "testRecursiveFunction" testRecursiveFunction
    , testCase "testLoop1" testLoop1
    , testCase "testLoop2" testLoop2
    , testCase "testLoop3" testLoop3
    , testCase "testLoop4" testLoop4
    , testCase "testLoop5" testLoop5
    , testCase "testLoopWithinFunction" testLoopWithinFunction
    , testCase "testLoopWithinFunction2" testLoopWithinFunction2
    , testCase "testLoopWithinSwitch" testLoopWithinSwitch
    , testCase "testLoopNested" testLoopNested
    ]
