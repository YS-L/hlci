import           Test.Tasty                    (defaultMain, testGroup)

import qualified Language.LOLCODE.Parser.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Language.LOLCODE.Parser.Tests.tests
    ]
