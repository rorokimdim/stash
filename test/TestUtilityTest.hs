module TestUtilityTest where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T

import qualified Cipher
import qualified TestUtility as TU
import qualified TextTransform as TT

import Types

tests :: TestTree
tests = testGroup "TestUtility Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup
  "Unit Tests"
  [ testCase "Tests for randomPlainNodes" $ do
    let (order, depth) = (2, 9)
    pnodes <- TU.randomPlainNodes 2 9
    assertEqual "Number of nodes should be order ^ (depth + 1) - 1" (order ^ (depth + 1) - 1) (length pnodes)
  , unitTestsForTextFormat OrgText
  , unitTestsForTextFormat MarkdownText
  ]

unitTestsForTextFormat :: TextFormat -> TestTree
unitTestsForTextFormat format = testGroup
  ("Unit Tests for " ++ show format)
  [ testCase ("Tests for randomTitle " ++ show format) $ do
    rtitles <- mapM (TU.randomTitle format) [0 .. 20]
    let depths = map (TT.depth format) rtitles
    assertEqual "depths" depths [0 .. 20]
  , testCase ("Tests for randomText " ++ show format) $ do
    let
      numTopTitles = 4
      depths       = [1 .. 4]
    xss <- mapM (\d -> TU.randomText format numTopTitles 1 d d) depths
    let counts = zipWith (\xs d -> length [ x | x <- xs, TT.depth format x == d ]) xss depths
    assertEqual "count of titles" [ numTopTitles * (d ^ (d - 1)) | d <- depths ] counts
  ]
