module DBTest where

import System.Environment (setEnv)

import Test.Tasty
import Test.Tasty.HUnit

import qualified DB.Internal as DB
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Types

tests :: TestTree
tests = testGroup "DB Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup
  "Unit Tests"
  [ testCase "Basic tests" $ do
    let ekey = "test-ekey"
    DB.runInMemoryDB $ \conn -> do
      DB.bootstrap_ conn ekey
      nid <- DB.addNode_ conn ekey 0 "a" "apple"
      assertEqual "addNode" 1 nid
      pnode <- DB.getPlainNodeById_ conn ekey nid
      assertEqual "getPlainNodeById" (__id <$> pnode) (Just nid)
      nids <- DB.save_ conn ekey 0 ["a", "b", "c"] "cherry"
      assertEqual "save" [1, 2, 3] nids
      nids <- DB.getIdsInPath_ conn 0 ["a", "b", "c"]
      assertEqual "getIdsInPath" [1, 2, 3] nids
      DB.updateNode_ conn ekey 3 "c" "cantaloupe"
      value <- DB.getValueById conn ekey 3
      assertEqual "getValueById" (Just "cantaloupe") value
      value <- DB.retrieve_ conn ekey 0 ["a", "b", "c"]
      assertEqual "retrieve" (Just "cantaloupe") value
      nid <- DB.lookupId_ conn 2 "c"
      assertEqual "lookupId" (Just 3) nid
      DB.setConfig_ conn "haskell" "awesome"
      value <- DB.getConfig_ conn "haskell"
      assertEqual "getConfig" value "awesome"
      nodes <- DB.getNodes_ conn 0
      assertEqual "getNodes" [1] [ _id n | n <- nodes ]
      nodes <- DB.getAllNodes_ conn
      assertEqual "getAllNodes" [1, 2, 3] [ _id n | n <- nodes ]
      path <- DB.getPath_ conn ekey 3
      assertEqual "getPath" ["a", "b", "c"] path
      (PlainTree hmap) <- DB.getPlainTree_ conn ekey 0
      assertEqual "getPlainTree" 1 (HM.size hmap)
      idsToDelete <- DB.getIds_ conn 2
      assertEqual "getIds" [2, 3] idsToDelete
      DB.deleteNodes_ conn idsToDelete
      nodes <- DB.getAllNodes_ conn
      assertEqual "deleteNodes" [1] [ _id n | n <- nodes ]
      decryptedNodes <- DB.decryptNodes ekey nodes
      assertEqual "decryptNodes" [("a", "apple")] [ (__key n, __value n) | n <- decryptedNodes ]
  , testCase "Tests for renaming node" $ do
    let ekey = "test-ekey"
    DB.runInMemoryDB $ \conn -> do
      DB.bootstrap_ conn ekey
      nidApple  <- DB.addNode_ conn ekey 0 "a" "apple"
      nidOrange <- DB.addNode_ conn ekey 0 "o" "orange"
      assertEqual "addNode" 1 nidApple
      assertEqual "addNode" 2 nidOrange
      result <- DB.renameNode_ conn ekey nidApple "microsoft"
      assertEqual "renameNode result" True result
      pnode <- DB.getPlainNodeById_ conn ekey nidApple
      assertEqual "renameNode" (Just "microsoft") (__key <$> pnode)
      result <- DB.renameNode_ conn ekey nidOrange "microsoft"
      assertEqual "renameNode result" False result
  , testCase "Tests for Node History" $ do
    let ekey = "test-ekey"
    DB.runInMemoryDB $ \conn -> do
      DB.bootstrap_ conn ekey
      nid <- DB.addNode_ conn ekey 0 "a" "apple"
      assertEqual "addNode" 1 nid
      DB.updateNode_ conn ekey 1 "a" "avocado"
      DB.updateNode_ conn ekey 1 "a" "apricot"
      DB.updateNode_ conn ekey 1 "a" "acerola"
      nodes <- DB.getAllPlainNodeVersions_ conn ekey 1
      assertEqual "getAllPlainNodeVersions" ["acerola", "apricot", "avocado", "apple"] [ __value n | n <- nodes ]
      DB.deleteNodes_ conn [1]
      nodes <- DB.getAllPlainNodeVersions_ conn ekey 1
      assertEqual "deleteNodes" 0 $ length nodes
  ]
