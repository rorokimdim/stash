module CipherTest where

import Control.Monad (forM)

import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.HashSet as Set
import qualified Data.Text as T
import qualified Test.Tasty.QuickCheck as QC

import Cipher (decrypt, encrypt, generateHashSalt, hash, maxEncryptionKeyLength)

tests :: TestTree
tests = testGroup "Cipher Tests" [quickCheckTests, unitTests]

quickCheckTests :: TestTree
quickCheckTests = testGroup
  "QuickCheck Tests"
  [ QC.testProperty "Encrypted value decrypts to original value"
  $ QC.withMaxSuccess 200
  $ \ekey value -> QC.ioProperty $ do
      let trimmedKey = T.take maxEncryptionKeyLength ekey
      encrypted <- encrypt trimmedKey value
      decrypted <- decrypt trimmedKey encrypted
      return $ decrypted == value
  , QC.testProperty "Hash should be unique by salt" $ \value -> QC.ioProperty $ do
    salts <- forM [1 .. 20] $ \x -> QC.generate QC.arbitrary :: IO T.Text
    let uniqueSalts = Set.fromList salts
    let hashes = Set.fromList [ hash s value | s <- Set.toList uniqueSalts ]
    return $ Set.size hashes == Set.size uniqueSalts
  ]

unitTests :: TestTree
unitTests = testGroup
  "Unit Tests"
  [ testCase "Tests for hash" $ do
    assertBool "same salt and value" $ hash "s0" "v0" == hash "s0" "v0"
    assertBool "same salt and different value" $ hash "s0" "v0" /= hash "s0" "v1"
    assertBool "different salt and same value" $ hash "s0" "v0" /= hash "s1" "v0"
    assertBool "different salt and different value" $ hash "s0" "v0" /= hash "s1" "v1"
  , testCase "Tests for generateHashSalt" $ do
    s0 <- generateHashSalt
    s1 <- generateHashSalt
    assertBool "Each call should yield different value" $ s0 /= s1
  , testCase "Tests for encrypt and decrypt" $ do
    e0 <- encrypt "k0" "m0"
    e1 <- encrypt "k0" "m0"
    assertBool "Each call should yield different value" $ e0 /= e1
    m0 <- decrypt "k0" e0
    m1 <- decrypt "k0" e1
    assertBool "Message encrypted with same key should always decrypt to same value" $ m0 == m1
    e2 <- encrypt "k2" "m2"
    e3 <- encrypt "k2" "m3"
    assertBool "Different messages should encrypt to different values" $ e2 /= e3
  ]
