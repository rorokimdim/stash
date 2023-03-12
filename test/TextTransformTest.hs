module TextTransformTest where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T

import qualified Cipher
import qualified TestUtility as TU
import qualified TextTransform as TT

import Types

tests :: TestTree
tests = testGroup "TextTransform Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [unitTestsForTextFormat OrgText, unitTestsForTextFormat MarkdownText]

unitTestsForTextFormat :: TextFormat -> TestTree
unitTestsForTextFormat format = testGroup
  ("Unit Tests for " ++ show format)
  [ testCase ("Tests for toTitle " ++ show format) $ do
    assertBool "empty string, depth 0" $ TT.toTitle format 0 "" == ""
    assertBool "empty string, depth 1" $ TT.toTitle format 1 "" == T.replicate 1 fchar <> " "
    assertBool "some string, depth 0" $ TT.toTitle format 0 "some string" == T.replicate 0 fchar <> "some string"
    assertBool "some string, depth 1" $ TT.toTitle format 1 "some string" == T.replicate 1 fchar <> " some string"
    assertBool "some string, depth 2" $ TT.toTitle format 2 "some string" == T.replicate 2 fchar <> " some string"
    assertBool "some string, depth 5" $ TT.toTitle format 5 "some string" == T.replicate 5 fchar <> " some string"
  , testCase ("Tests for untitleText " ++ show format) $ do
    assertBool "empty string" $ TT.untitleText format "" == ""
    assertBool "some string" $ TT.untitleText format "some string" == "some string"
    assertBool "some string, depth 2" $ TT.untitleText format (T.replicate 2 fchar <> " some string") == "some string"
    assertBool "some string, depth 5" $ TT.untitleText format (T.replicate 5 fchar <> " some string") == "some string"
  , testCase "Tests for depth OrgText" $ do
    assertBool "empty string" $ TT.depth format "" == 0
    assertBool "some string" $ TT.depth format "some string" == 0
    assertBool "some string, depth 1" $ TT.depth format (T.replicate 1 fchar <> " some string") == 1
    assertBool "some string, depth 2" $ TT.depth format (T.replicate 2 fchar <> " some string") == 2
    assertBool "some string, depth 5" $ TT.depth format (T.replicate 5 fchar <> " some string") == 5
  , testCase ("Tests for toText " ++ show format) $ do
    plainNodes <- TU.randomPlainNodes 10 1
    let t = TT.toText format plainNodes
    assertEqual "One level deep" 10 (length $ [ x | x <- T.lines t, TT.depth format x == 1 ])
    plainNodes <- TU.randomPlainNodes 2 4
    let t = TT.toText format plainNodes
    assertEqual "Two level deep"  4  (length $ [ x | x <- T.lines t, TT.depth format x == 2 ])
    assertEqual "Four level deep" 16 (length $ [ x | x <- T.lines t, TT.depth format x == 4 ])
  , testCase ("Tests for walkText " ++ show format) $ do
    plainNodes <- TU.randomPlainNodes 2 4
    let t = TT.toText format plainNodes
    finalState <- TT.walkText 0 format t $ \s ks body -> return $ s + 1
    assertEqual "Total walk calls" 31 finalState
  ]
 where
  fchar = case format of
    OrgText      -> "*"
    MarkdownText -> "#"
