import Test.Tasty
import Test.Tasty.HUnit

import qualified CipherTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [CipherTest.tests]
