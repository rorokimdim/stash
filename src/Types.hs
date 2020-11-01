module Types where

import qualified Data.Text as T

import Data.ByteString (ByteString)
import Data.Time (UTCTime)

type EncryptedKey = ByteString
type EncryptedValue = ByteString
type EncryptionKey = String
type NodeId = Integer
type ParentId = Integer
type PlainKey = T.Text
type PlainValue = T.Text

data Node = Node {
  _id :: NodeId,
  _parent :: NodeId,
  _hkey :: String,
  _hvalue :: String,
  _key :: EncryptedKey,
  _value :: EncryptedValue,
  _version :: Integer,
  _created :: UTCTime,
  _modified :: UTCTime
} deriving (Show)

data PlainNode = PlainNode {
  __id :: NodeId,
  __parent :: NodeId,
  __hkey :: String,
  __hvalue :: String,
  __key :: PlainKey,
  __value :: PlainValue,
  __version :: Integer,
  __created :: UTCTime,
  __modified :: UTCTime
} deriving (Show)
