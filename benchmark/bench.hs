import Criterion.Main

import qualified Data.Text as T

import qualified TestUtility as TU
import qualified TextTransform as TT

import Types

main :: IO ()
main = do
  orgText      <- T.intercalate "\n" <$> TU.randomText OrgText 200 1 0 4
  markdownText <- T.intercalate "\n" <$> TU.randomText MarkdownText 200 1 0 4
  pnodes       <- TU.randomPlainNodes 2 10
  let walker s ks body = return $ s + 1
  defaultMain
    [ bgroup
      "walkText"
      [ bench "OrgText" $ whnfIO (TT.walkText 0 OrgText orgText walker)
      , bench "MarkdownText" $ whnfIO (TT.walkText 0 MarkdownText markdownText walker)
      ]
    , bgroup
      "toText"
      [bench "OrgText" $ whnf (TT.toText OrgText) pnodes, bench "MarkdownText" $ whnf (TT.toText MarkdownText) pnodes]
    ]
