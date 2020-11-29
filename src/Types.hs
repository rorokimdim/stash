module Types where

import Data.Aeson ((.=), (.:), FromJSON, ToJSON, object, parseJSON, toJSON, withObject)

import Data.ByteString (ByteString)
import Data.Time (UTCTime)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

type EncryptedKey = ByteString
type EncryptedValue = ByteString
type EncryptionKey = T.Text
type HashSalt = T.Text
type NodeId = Integer
type ParentId = Integer
type PlainKey = T.Text
type PlainValue = T.Text

newtype PlainTree = PlainTree (HM.HashMap PlainKey (PlainValue, [PlainTree])) deriving (Show)

data TextFormat = JSONText | OrgText | MarkdownText
instance Show TextFormat where
  show JSONText     = "json"
  show OrgText      = "org"
  show MarkdownText = "markdown"

data Node = Node {
  _id :: NodeId,
  _parent :: NodeId,
  _hkey :: T.Text,
  _hvalue :: T.Text,
  _key :: EncryptedKey,
  _value :: EncryptedValue,
  _version :: Integer,
  _created :: UTCTime,
  _modified :: UTCTime
} deriving (Show)

data PlainNode = PlainNode {
  __id :: NodeId,
  __parent :: NodeId,
  __hkey :: T.Text,
  __hvalue :: T.Text,
  __key :: PlainKey,
  __value :: PlainValue,
  __version :: Integer,
  __created :: UTCTime,
  __modified :: UTCTime
} deriving (Show)

instance ToJSON PlainNode where
  toJSON PlainNode { __id = nid, __parent = pid, __hkey = hkey, __hvalue = hvalue, __key = key, __value = value, __version = version, __created = created, __modified = modified }
    = object
      [ "id" .= nid
      , "parent-id" .= pid
      , "hkey" .= hkey
      , "hvalue" .= hvalue
      , "key" .= key
      , "value" .= value
      , "version" .= version
      , "created" .= created
      , "modified" .= modified
      ]

instance FromJSON PlainNode where
  parseJSON = withObject "PlainNode" $ \obj -> do
    nid      <- obj .: "id"
    pid      <- obj .: "parent-id"
    hkey     <- obj .: "hkey"
    hvalue   <- obj .: "hvalue"
    key      <- obj .: "key"
    value    <- obj .: "value"
    version  <- obj .: "version"
    created  <- obj .: "created"
    modified <- obj .: "modified"
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

instance ToJSON PlainTree where
  toJSON (PlainTree m) = toJSON m
