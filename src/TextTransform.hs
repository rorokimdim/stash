module TextTransform
  ( toText
  , walkText
  , TextFormat(..)
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T

import Types

type Depth = Int
type Title = T.Text
type Body = T.Text

toTitle :: TextFormat -> Depth -> Title -> Title
toTitle MarkdownText depth t = T.concat [T.pack $ replicate depth '#', " ", t, "\n"]
toTitle OrgText      depth t = T.concat [T.pack $ replicate depth '*', " ", t, "\n"]

toBody :: TextFormat -> Depth -> Body -> Body
toBody _ _ t | T.null t = T.empty
toBody _ _ t            = T.append t "\n"

toText :: TextFormat -> [PlainNode] -> T.Text
toText format plainNodes = T.concat $ map (fromPlainNode 1 T.empty) topNodes
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
      body   = toBody format depth $ __value n
      ntext  = T.append t $ if T.null body then title else T.append title body
      cids   = HM.lookupDefault [] (__id n) childMap
      cnodes = [ idMap HM.! cid | cid <- cids ]
    in if null cnodes
      then ntext
      else T.append ntext $ T.concat $ map (fromPlainNode (depth + 1) T.empty) cnodes
  topIds   = HM.lookupDefault [] 0 childMap
  topNodes = [ idMap HM.! tid | tid <- topIds ]

depth :: TextFormat -> T.Text -> Int
depth MarkdownText = T.length . T.takeWhile (== '#')
depth OrgText      = T.length . T.takeWhile (== '*')

untitleText :: TextFormat -> T.Text -> T.Text
untitleText MarkdownText = T.strip . T.dropWhile (== '#')
untitleText OrgText      = T.strip . T.dropWhile (== '*')

walkText :: TextFormat -> T.Text -> ([PlainKey] -> Body -> IO ()) -> IO ()
walkText format t f = do
  let lines = T.lines t
  let
    process (x : xs) titles bodies = do
      let d       = depth format x
      let isTitle = d > 0
      if isTitle
        then do
          if null titles then pure () else f titles (T.concat bodies)
          let newTitles = take (d - 1) titles ++ [untitleText format x]
          process xs newTitles []
        else process xs titles (bodies ++ [T.strip x])
    process [] []     _      = return ()
    process [] titles bodies = f titles (T.concat bodies)
  process lines [] []
