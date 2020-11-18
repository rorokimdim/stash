module TestUtility
  ( randomPlainNode
  , randomPlainNodes
  , randomLine
  , randomTitle
  , randomText
  , randomBody
  )
where

import Control.Monad (replicateM)
import Data.Time (getCurrentTime)
import System.Random (randomRIO)

import qualified Data.Text as T
import qualified Test.RandomStrings as RS

import qualified Cipher

import Types

-- |Gets a random PlainNode with given parent-id and node-id.
randomPlainNode :: ParentId -> NodeId -> IO PlainNode
randomPlainNode pid nid = do
  keySize <- randomRIO (1, 31)
  key     <- T.take keySize <$> randomLine
  value   <- randomBody
  version <- randomRIO (1, 100)
  let hkey   = Cipher.hash "salt" key
  let hvalue = Cipher.hash "salt" value
  created  <- getCurrentTime
  modified <- getCurrentTime
  return PlainNode
    { __id       = nid
    , __parent   = pid
    , __hkey     = hkey
    , __hvalue   = hvalue
    , __key      = key
    , __value    = value
    , __version  = version
    , __created  = created
    , __modified = modified
    }

-- |Gets a tree of plain-nodes with provided depth and order.
--
-- order is the number of children per node
-- number of nodes at each level = order ^ level
-- total number of nodes = order ^ (depth + 1) - 1
randomPlainNodes :: Int -> Int -> IO [PlainNode]
randomPlainNodes order depth = do
  let
    pids :: [ParentId]
    pids = concatMap (replicate order) [0 ..]
  let
    fns :: [NodeId -> IO PlainNode]
    fns = map randomPlainNode pids
  sequence [ fn (toInteger nid) | (fn, nid) <- zip fns [1 .. order ^ (depth + 1) - 1] ]

-- |Gets random text for given format.
randomText :: TextFormat -> Int -> Int -> Int -> Int -> IO [T.Text]
randomText _ 0 _ _ _ = return []
randomText format numTitles depth minDepth maxDepth | depth > maxDepth = return []
randomText format numTitles depth minDepth maxDepth = do
  title        <- randomTitle format depth
  body         <- randomBody

  numSubTitles <- randomRIO (minDepth, maxDepth)
  children     <- randomText format numSubTitles (depth + 1) minDepth maxDepth
  others       <- randomText format (numTitles - 1) depth minDepth maxDepth

  return $ title : body : (children ++ others)

-- |Gets random title for given format.
randomTitle :: TextFormat -> Int -> IO T.Text
randomTitle format depth = do
  rtext <- randomLine
  let
    fc = case format of
      OrgText      -> '*'
      MarkdownText -> '#'
  if depth == 0 then return rtext else return $ T.pack (replicate depth fc) <> " " <> rtext

-- |Gets random body of text
randomBody :: IO T.Text
randomBody = do
  n     <- randomRIO (minLinesPerBody, maxLinesPerBody)
  lines <- replicateM n randomLine
  return $ T.intercalate "\n" lines

-- |Gets a random line of text
randomLine :: IO T.Text
randomLine = do
  n  <- randomRIO (minWordsPerLine, maxWordsPerLine)
  xs <- RS.randomStringsLen (RS.randomWord RS.randomASCII) (minWordSize, maxWordSize) n
  return $ T.intercalate " " [ T.pack x | x <- xs ]

minWordSize :: Int
minWordSize = 1

maxWordSize :: Int
maxWordSize = 20

minWordsPerLine :: Int
minWordsPerLine = 1

maxWordsPerLine :: Int
maxWordsPerLine = 20

minLinesPerBody :: Int
minLinesPerBody = 0

maxLinesPerBody :: Int
maxLinesPerBody = 10
