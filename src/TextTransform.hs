module TextTransform
  ( toMarkdownText
  , toOrgText
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T

import Types

type Depth = Int
type Title = T.Text
type Body = T.Text

toMarkdownTitle :: Depth -> Title -> Title
toMarkdownTitle depth t = T.concat [T.pack $ replicate depth '#', " ", t, "\n"]

toMarkdownBody :: Depth -> Body -> Body
toMarkdownBody _ t | T.null t = T.empty
toMarkdownBody _ t            = T.append t "\n"

toMarkdownText :: [PlainNode] -> T.Text
toMarkdownText = toText toMarkdownTitle toMarkdownBody

toOrgTitle :: Depth -> Title -> Title
toOrgTitle depth t = T.concat [T.pack $ replicate depth '*', " ", t, "\n"]

toOrgBody :: Depth -> Body -> Body
toOrgBody _ t | T.null t = T.empty
toOrgBody _ t            = T.append t "\n"

toOrgText :: [PlainNode] -> T.Text
toOrgText = toText toOrgTitle toOrgBody

toText :: (Depth -> Title -> Title) -> (Depth -> Body -> Body) -> [PlainNode] -> T.Text
toText tfTitle tfBody plainNodes = T.concat $ map (fromPlainNode 1 T.empty) topNodes
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
      title  = tfTitle depth $ __key n
      body   = tfBody depth $ __value n
      ntext  = T.append t $ if T.null body then title else T.append title body
      cids   = HM.lookupDefault [] (__id n) childMap
      cnodes = [ idMap HM.! cid | cid <- cids ]
    in if null cnodes
      then ntext
      else T.append ntext $ T.concat $ map (fromPlainNode (depth + 1) T.empty) cnodes
  topIds   = HM.lookupDefault [] 0 childMap
  topNodes = [ idMap HM.! tid | tid <- topIds ]
