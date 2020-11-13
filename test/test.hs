import Test.Tasty
import Test.Tasty.HUnit

import qualified CipherTest
import qualified TestUtilityTest
import qualified TextTransformTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [CipherTest.tests, TextTransformTest.tests, TestUtilityTest.tests]
