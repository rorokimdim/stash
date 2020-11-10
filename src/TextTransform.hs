module TextTransform
  ( toText
  , walkText
  , TextFormat(..)
  )
where

import Data.List (sortBy)

import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T

import Types

type Depth = Int
type Title = T.Text
type Body = T.Text

-- |Transforms a text value to titile for given format.
toTitle :: TextFormat -> Depth -> Title -> Title
toTitle MarkdownText depth t = T.concat [T.pack $ replicate depth '#', " ", t]
toTitle OrgText      depth t = T.concat [T.pack $ replicate depth '*', " ", t]

-- |Sorts a list of plain-nodes in lexicographical order.
sortPlainNodesByKey :: [PlainNode] -> [PlainNode]
sortPlainNodesByKey ns = sortBy f ns where f n1 n2 = __key n1 `compare` __key n2

-- |Transforms a list of plain-nodes into text in given format.
toText :: TextFormat -> [PlainNode] -> T.Text
toText format plainNodes = T.intercalate "\n" $ map (fromPlainNode 1 T.empty) topNodes
 where
  idMap    = HM.fromList [ (__id n, n) | n <- plainNodes ]
  childMap = buildChildMap HM.empty plainNodes
  buildChildMap m []       = m
  buildChildMap m (n : ns) = buildChildMap (HM.alter f (__parent n) m) ns
   where
    f Nothing     = Just [__id n]
    f (Just cids) = Just (__id n : cids)
  fromPlainNode depth t n =
    let
      title  = toTitle format depth $ __key n
      body   = __value n
      ntext  = T.strip $ T.intercalate "\n" [t, title, body]
      cids   = HM.lookupDefault [] (__id n) childMap
      cnodes = sortPlainNodesByKey [ idMap HM.! cid | cid <- cids ]
    in if null cnodes
      then ntext
      else T.strip $ T.intercalate
        "\n"
        [ntext, T.intercalate "\n" $ map (fromPlainNode (depth + 1) T.empty) cnodes]
  topIds   = HM.lookupDefault [] 0 childMap
  topNodes = sortPlainNodesByKey [ idMap HM.! tid | tid <- topIds ]

-- |Finds depth of a string in given format.
depth :: TextFormat -> T.Text -> Int
depth MarkdownText = T.length . T.takeWhile (== '#')
depth OrgText      = T.length . T.takeWhile (== '*')

-- |Converts given string into non-title string.
untitleText :: TextFormat -> T.Text -> T.Text
untitleText MarkdownText = T.strip . T.dropWhile (== '#')
untitleText OrgText      = T.strip . T.dropWhile (== '*')

-- |Reads provided text and calls given function with each pair of [PlainKey], Body found.
walkText :: s -> TextFormat -> T.Text -> (s -> [PlainKey] -> Body -> IO s) -> IO s
walkText state format t f = do
  let lines = T.lines t
  let
    process state (x : xs) titles bodies = do
      let d       = depth format x
      let isTitle = d > 0
      if isTitle
        then do
          if null titles
            then do
              process state xs [untitleText format x] []
            else do
              newState <- f state titles $ T.intercalate "\n" bodies
              let newTitles = take (d - 1) titles ++ [untitleText format x]
              process newState xs newTitles []
        else process state xs titles (bodies ++ [T.strip x])
    process state [] []     _      = return state
    process state [] titles bodies = f state titles $ T.intercalate "\n" bodies
  process state lines [] []
