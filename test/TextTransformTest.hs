module TextTransformTest where

import Data.Time (UTCTime)
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import qualified Test.Tasty.QuickCheck as QC

import Types
import qualified Cipher
import qualified TextTransform as TT

tests :: TestTree
tests = testGroup "TextTransform Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup "Unit Tests" [unitTestsForTextFormat OrgText, unitTestsForTextFormat MarkdownText]

unitTestsForTextFormat :: TextFormat -> TestTree
unitTestsForTextFormat format = testGroup
  ("Unit Tests for " ++ show format)
  [ testCase ("Tests for toTitle " ++ show format) $ do
    assertBool "empty string, depth 0" $ TT.toTitle format 0 "" == ""
    assertBool "empty string, depth 1" $ TT.toTitle format 1 "" == T.replicate 1 fchar <> " "
    assertBool "some string, depth 0"
      $  TT.toTitle format 0 "some string"
      == T.replicate 0 fchar
      <> "some string"
    assertBool "some string, depth 1"
      $  TT.toTitle format 1 "some string"
      == T.replicate 1 fchar
      <> " some string"
    assertBool "some string, depth 2"
      $  TT.toTitle format 2 "some string"
      == T.replicate 2 fchar
      <> " some string"
    assertBool "some string, depth 5"
      $  TT.toTitle format 5 "some string"
      == T.replicate 5 fchar
      <> " some string"
  , testCase ("Tests for untitleText " ++ show format) $ do
    assertBool "empty string" $ TT.untitleText format "" == ""
    assertBool "some string" $ TT.untitleText format "some string" == "some string"
    assertBool "some string, depth 2"
      $  TT.untitleText format (T.replicate 2 fchar <> " some string")
      == "some string"
    assertBool "some string, depth 5"
      $  TT.untitleText format (T.replicate 5 fchar <> " some string")
      == "some string"
  , testCase "Tests for depth OrgText" $ do
    assertBool "empty string" $ TT.depth format "" == 0
    assertBool "some string" $ TT.depth format "some string" == 0
    assertBool "some string, depth 1" $ TT.depth format (T.replicate 1 fchar <> " some string") == 1
    assertBool "some string, depth 2" $ TT.depth format (T.replicate 2 fchar <> " some string") == 2
    assertBool "some string, depth 5" $ TT.depth format (T.replicate 5 fchar <> " some string") == 5
  , testCase ("Tests for toText " ++ show format) $ do
    let countDepth1 = 10
    let countDepth2 = 5
    plainNodes0 <- generatePlainNodes 0 countDepth1
    let t = TT.toText format plainNodes0
    assertEqual "One level deep" (2 * countDepth1) (length $ T.lines t)
    plainNodes1 <- generatePlainNodes 1 5
    let t       = TT.toText format $ plainNodes0 ++ plainNodes1
    let titles1 = filter (\x -> TT.depth format x == 1) $ T.lines t
    let titles2 = filter (\x -> TT.depth format x == 2) $ T.lines t
    let titles3 = filter (\x -> TT.depth format x == 3) $ T.lines t
    assertEqual "Number of top level titles" countDepth1 $ length titles1
    assertEqual "Number of second level titles" countDepth2 $ length titles2
    assertEqual "Number of third level titles" 0 $ length titles3
    assertEqual "Count two level deep" (2 * (countDepth1 + countDepth2)) (length $ T.lines t)
  , testCase ("Tests for walkText " ++ show format) $ do
    plainNodes <- generatePlainNodes 0 10
    let t = TT.toText format plainNodes
    finalState <- TT.walkText 0 format t $ \s ks body -> return $ s + 1
    assertEqual "Total walk calls" 10 finalState
  ]
 where
  fchar = case format of
    OrgText      -> "*"
    MarkdownText -> "#"

generatePlainNodes :: ParentId -> Integer -> IO [PlainNode]
generatePlainNodes _   0 = return []
generatePlainNodes pid n = do
  let
    validRandomText = do
      rtext <- QC.generate QC.arbitrary :: IO T.Text
      return $ T.append "rtext" $ T.filter (/= '\n') rtext
  key     <- validRandomText
  value   <- validRandomText
  version <- QC.generate QC.arbitrary :: IO Integer
  let hkey   = Cipher.hash "salt" key
  let hvalue = Cipher.hash "salt" value
  created  <- QC.generate QC.arbitrary :: IO UTCTime
  modified <- QC.generate QC.arbitrary :: IO UTCTime
  others   <- generatePlainNodes pid $ n - 1
  return
    $ PlainNode
        { __id       = pid * n + n
        , __parent   = pid
        , __hkey     = hkey
        , __hvalue   = hvalue
        , __key      = key
        , __value    = value
        , __version  = version
        , __created  = created
        , __modified = modified
        }
    : others
