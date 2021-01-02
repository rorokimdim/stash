module BabashkaPod where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Data.Aeson ((.:), (.:?), FromJSON, decode, encode, parseJSON, withObject)
import Data.Maybe (isNothing)
import System.IO (hFlush, isEOF, stdout)

import qualified Data.BEncode as BE
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import qualified DB
import qualified IOUtils
import qualified Version

import Types

data PodState = PodState {
  _ekey :: EncryptionKey,
  _stashPath :: FilePath,
  _authenticated :: Bool,
  _shutdown :: Bool
}

data InitializationArg = InitializationArg {
  __ekey :: EncryptionKey,
  __stashPath :: FilePath,
  __createStashIfMissing :: Bool
}

instance FromJSON InitializationArg where
  parseJSON = withObject "InitializationArg" $ \obj -> do
    ekey                 <- obj .: "encryption-key"
    stashPath            <- obj .: "stash-path"
    createStashIfMissing <- obj .:? "create-stash-if-missing"
    return InitializationArg
      { __ekey                 = ekey
      , __stashPath            = stashPath
      , __createStashIfMissing = Just True == createStashIfMissing
      }

type PodRequestId = BSLazy.ByteString
type InvokeVar = BSLazy.ByteString
type Args = BSLazy.ByteString

data InvokeRequest = InvokeRequest InvokeVar PodRequestId Args

continueState :: PodState -> BE.BEncode -> IO (PodState, BE.BEncode)
continueState s b = return (s, b)

handleInitRequest :: PodState -> PodRequestId -> Args -> IO (PodState, BE.BEncode)
handleInitRequest s rid args = do
  let
    invalid    = constructBencodeError rid "Invalid initialization args" (BE.BString args)
    parsedArgs = decode args :: Maybe [InitializationArg]

  case parsedArgs of
    Nothing -> return (s, invalid)
    Just xs -> if null xs
      then return (s, invalid)
      else do
        let
          obj                  = head xs
          ekey                 = __ekey obj
          stashPath            = __stashPath obj
          createStashIfMissing = __createStashIfMissing obj

        DB.setDBPath stashPath
        validationResult <- DB.validateDBPath stashPath

        case validationResult of
          DB.NonExistentDBFile -> do
            if createStashIfMissing
              then do
                IOUtils.createMissingDirectories stashPath
                DB.bootstrap ekey
                continueState s { _ekey = ekey, _stashPath = stashPath, _authenticated = True }
                  $ BE.BDict
                  $ Map.fromList
                      [ ("id"    , BE.BString rid)
                      , ("value" , BE.BString "true")
                      , ("status", BE.BList [BE.BString "done"])
                      ]
              else continueState s $ constructBencodeError
                rid
                ("stash file "
                <> C.pack stashPath
                <> " does not exist. Pass in create-stash-if-missing=true to create it. Or use `stash create`."
                )
                (BE.BString "false")
          DB.InvalidDBFile -> continueState s $ constructBencodeError
            rid
            ("Invalid stash file " <> C.pack stashPath)
            (BE.BString "false")
          DB.ValidDBFile -> do
            isValid <- DB.checkEncryptionKey ekey
            continueState s { _ekey = ekey, _stashPath = stashPath, _authenticated = isValid }
              $ BE.BDict
              $ Map.fromList
                  [ ("id"    , BE.BString rid)
                  , ("value", BE.BString (if isValid then "true" else "false"))
                  , ("status", BE.BList [BE.BString "done"])
                  ]

handleNodesRequest :: PodState -> PodRequestId -> Args -> IO (PodState, BE.BEncode)
handleNodesRequest s rid args = do
  let
    ekey       = _ekey s
    parsedArgs = decode args :: Maybe [ParentId]

  plainNodes <- case parsedArgs of
    Nothing    -> DB.getAllPlainNodes ekey
    Just [pid] -> DB.getPlainNodes ekey pid
    _          -> return []

  continueState s $ BE.BDict $ Map.fromList
    [ ("id"    , BE.BString rid)
    , ("value" , BE.BString $ encode plainNodes)
    , ("status", BE.BList [BE.BString "done"])
    ]

handleNodeVersionsRequest :: PodState -> PodRequestId -> Args -> IO (PodState, BE.BEncode)
handleNodeVersionsRequest s rid args = do
  let
    ekey       = _ekey s
    parsedArgs = decode args :: Maybe [NodeId]
    invalid    = constructBencodeError rid "Invalid node id" (BE.BString "null")

  case parsedArgs of
    Just [nid] -> do
      plainNodes <- DB.getAllPlainNodeVersions ekey nid
      continueState s $ BE.BDict $ Map.fromList
        [ ("id"    , BE.BString rid)
        , ("value" , BE.BString $ encode plainNodes)
        , ("status", BE.BList [BE.BString "done"])
        ]
    _ -> return (s, invalid)

handleTreeRequest :: PodState -> PodRequestId -> Args -> IO (PodState, BE.BEncode)
handleTreeRequest s rid args = do
  let
    ekey       = _ekey s
    parsedArgs = decode args :: Maybe [ParentId]
    pid        = case parsedArgs of
      Just [x] -> x
      _        -> 0

  tree <- DB.getPlainTree ekey pid
  continueState s $ BE.BDict $ Map.fromList
    [ ("id"    , BE.BString rid)
    , ("value" , BE.BString $ encode tree)
    , ("status", BE.BList [BE.BString "done"])
    ]

handleKeysRequest :: PodState -> PodRequestId -> Args -> IO (PodState, BE.BEncode)
handleKeysRequest s rid args = do
  let
    ekey       = _ekey s
    invalid    = constructBencodeError rid "Invalid parent ids" (BE.BString "null")
    parsedArgs = decode args :: Maybe [ParentId]

  case parsedArgs of
    Nothing   -> return (s, invalid)
    Just pids -> do
      kss <- mapM (DB.getPlainKeys ekey) pids
      continueState s $ BE.BDict $ Map.fromList
        [ ("id"    , BE.BString rid)
        , ("value" , BE.BString $ encode $ concat kss)
        , ("status", BE.BList [BE.BString "done"])
        ]

handleGetRequest :: PodState -> PodRequestId -> Args -> IO (PodState, BE.BEncode)
handleGetRequest s rid args = do
  let
    ekey       = _ekey s
    invalid    = constructBencodeError rid "Invalid keys" (BE.BString args)
    parsedArgs = decode args :: Maybe [PlainKey]

  case parsedArgs of
    Nothing -> return (s, invalid)
    Just [] -> return (s, invalid)
    Just ks -> do
      value <- DB.retrieve ekey ks
      continueState s $ BE.BDict $ Map.fromList
        [ ("id"    , BE.BString rid)
        , ("value" , BE.BString $ encode value)
        , ("status", BE.BList [BE.BString "done"])
        ]

handleSetRequest :: PodState -> PodRequestId -> Args -> IO (PodState, BE.BEncode)
handleSetRequest s rid args = do
  let
    ekey    = _ekey s
    invalid = constructBencodeError
      rid
      "Invalid input; must be list of keys followed by value"
      (BE.BString args)
    parsedArgs = decode args :: Maybe [T.Text]

  case parsedArgs of
    Nothing  -> return (s, invalid)
    Just []  -> return (s, invalid)
    Just [x] -> return (s, invalid)
    Just xs  -> do
      value <- DB.save ekey (init xs) (last xs)
      continueState s $ BE.BDict $ Map.fromList
        [ ("id"    , BE.BString rid)
        , ("value" , BE.BString $ encode value)
        , ("status", BE.BList [BE.BString "done"])
        ]

handleAddRequest :: PodState -> PodRequestId -> Args -> IO (PodState, BE.BEncode)
handleAddRequest s rid args = do
  let
    ekey = _ekey s
    invalid =
      constructBencodeError rid "Invalid input; must be parent-id, key, value" (BE.BString "null")
    parsedArgs = decode args :: Maybe (ParentId, PlainKey, PlainValue)

  case parsedArgs of
    Nothing                  -> return (s, invalid)
    Just (pid, pkey, pvalue) -> do
      existingNodeId <- DB.lookupId pid pkey
      if isNothing existingNodeId
        then do
          nid <- DB.addNode ekey pid pkey pvalue
          continueState s $ BE.BDict $ Map.fromList
            [ ("id"    , BE.BString rid)
            , ("value" , BE.BString $ encode nid)
            , ("status", BE.BList [BE.BString "done"])
            ]
        else continueState s $ constructBencodeError rid "Key already exists" (BE.BString "null")

handleRenameRequest :: PodState -> PodRequestId -> Args -> IO (PodState, BE.BEncode)
handleRenameRequest s rid args = do
  let
    ekey = _ekey s
    invalid =
      constructBencodeError rid "Invalid input; must be node-id, new-name" (BE.BString "null")
    parsedArgs = decode args :: Maybe (NodeId, PlainKey)

  case parsedArgs of
    Nothing          -> return (s, invalid)
    Just (nid, pkey) -> do
      result <- DB.renameNode ekey nid pkey
      if result
        then continueState s $ BE.BDict $ Map.fromList
          [ ("id"    , BE.BString rid)
          , ("value" , BE.BString "true")
          , ("status", BE.BList [BE.BString "done"])
          ]
        else continueState s
          $ constructBencodeError rid "Invalid new name. Name already exists." (BE.BString "null")

handleUpdateRequest :: PodState -> PodRequestId -> Args -> IO (PodState, BE.BEncode)
handleUpdateRequest s rid args = do
  let
    ekey = _ekey s
    invalid = constructBencodeError rid "Invalid input; must be node-id, value" (BE.BString "null")
    parsedArgs = decode args :: Maybe (NodeId, PlainValue)

  case parsedArgs of
    Nothing            -> return (s, invalid)
    Just (nid, pvalue) -> do
      DB.updateNodeValue ekey nid pvalue
      continueState s $ BE.BDict $ Map.fromList
        [ ("id"    , BE.BString rid)
        , ("value" , BE.BString "true")
        , ("status", BE.BList [BE.BString "done"])
        ]

handleDeleteRequest :: PodState -> PodRequestId -> Args -> IO (PodState, BE.BEncode)
handleDeleteRequest s rid args = do
  let
    ekey       = _ekey s
    invalid    = constructBencodeError rid "Invalid node ids" (BE.BString "null")
    parsedArgs = decode args :: Maybe [NodeId]

  case parsedArgs of
    Nothing   -> return (s, invalid)
    Just nids -> do
      expandedIdss <- mapM DB.getIds nids
      DB.deleteNodes $ concat expandedIdss
      continueState s $ BE.BDict $ Map.fromList
        [ ("id"    , BE.BString rid)
        , ("value" , BE.BString "true")
        , ("status", BE.BList [BE.BString "done"])
        ]

handleVersionRequest :: PodState -> PodRequestId -> Args -> IO (PodState, BE.BEncode)
handleVersionRequest s rid args = do
  continueState s $ BE.BDict $ Map.fromList
    [ ("id"    , BE.BString rid)
    , ("value" , BE.BString (encode Version.appVersion))
    , ("status", BE.BList [BE.BString "done"])
    ]

handleInvokeRequest :: PodState -> InvokeRequest -> IO (PodState, BE.BEncode)
handleInvokeRequest s (InvokeRequest "pod.rorokimdim.stash/init" rid args) =
  handleInitRequest s rid args
handleInvokeRequest s (InvokeRequest _ rid args) | (not . _authenticated) s =
  continueState s $ constructBencodeError
    rid
    "Not Authenticated. A call to `init` must succeed first."
    (BE.BString args)
handleInvokeRequest s (InvokeRequest "pod.rorokimdim.stash/nodes" rid args) =
  handleNodesRequest s rid args
handleInvokeRequest s (InvokeRequest "pod.rorokimdim.stash/node-versions" rid args) =
  handleNodeVersionsRequest s rid args
handleInvokeRequest s (InvokeRequest "pod.rorokimdim.stash/tree" rid args) =
  handleTreeRequest s rid args
handleInvokeRequest s (InvokeRequest "pod.rorokimdim.stash/get" rid args) =
  handleGetRequest s rid args
handleInvokeRequest s (InvokeRequest "pod.rorokimdim.stash/set" rid args) =
  handleSetRequest s rid args
handleInvokeRequest s (InvokeRequest "pod.rorokimdim.stash/keys" rid args) =
  handleKeysRequest s rid args
handleInvokeRequest s (InvokeRequest "pod.rorokimdim.stash/add" rid args) =
  handleAddRequest s rid args
handleInvokeRequest s (InvokeRequest "pod.rorokimdim.stash/rename" rid args) =
  handleRenameRequest s rid args
handleInvokeRequest s (InvokeRequest "pod.rorokimdim.stash/update" rid args) =
  handleUpdateRequest s rid args
handleInvokeRequest s (InvokeRequest "pod.rorokimdim.stash/delete" rid args) =
  handleDeleteRequest s rid args
handleInvokeRequest s (InvokeRequest "pod.rorokimdim.stash/version" rid args) =
  handleVersionRequest s rid args
handleInvokeRequest s (InvokeRequest var rid _) =
  continueState s $ constructBencodeError rid "Invalid invoke request" (BE.BString var)

handleShutdownRequest :: PodState -> PodRequestId -> IO (PodState, BE.BEncode)
handleShutdownRequest s rid = do
  let
    result =
      BE.BDict $ Map.fromList [("id", BE.BString rid), ("status", BE.BList [BE.BString "done"])]
  return (s { _shutdown = True }, result)

handleRequest :: PodState -> BE.BEncode -> IO (PodState, BE.BEncode)
handleRequest s (BE.BDict d) = do
  let
    op              = Map.findWithDefault (BE.BString "") "op" d
    BE.BString rid  = Map.findWithDefault (BE.BString "") "id" d
    BE.BString args = Map.findWithDefault (BE.BString "") "args" d
  case op of
    (BE.BString "describe") -> return (s, podDescription rid)
    (BE.BString "invoke"  ) -> handleInvokeRequest s (InvokeRequest invokeVar rid args)
      where (BE.BString invokeVar) = Map.findWithDefault (BE.BString "") "var" d
    (BE.BString "shutdown") -> handleShutdownRequest s rid
    _                       -> return (s, constructBencodeError rid "Invalid op" op)

podDescription :: PodRequestId -> BE.BEncode
podDescription rid = BE.BDict $ Map.fromList
  [ ("format", BE.BString "json")
  , ("id"    , BE.BString rid)
  , ( "namespaces"
    , BE.BList
      [ BE.BDict $ Map.fromList
          [ ("name", BE.BString "pod.rorokimdim.stash")
          , ( "vars"
            , BE.BList
              [ BE.BDict $ Map.fromList [("name", BE.BString "init")]
              , BE.BDict $ Map.fromList [("name", BE.BString "nodes")]
              , BE.BDict $ Map.fromList [("name", BE.BString "node-versions")]
              , BE.BDict $ Map.fromList [("name", BE.BString "tree")]
              , BE.BDict $ Map.fromList [("name", BE.BString "get")]
              , BE.BDict $ Map.fromList [("name", BE.BString "keys")]
              , BE.BDict $ Map.fromList [("name", BE.BString "set")]
              , BE.BDict $ Map.fromList [("name", BE.BString "add")]
              , BE.BDict $ Map.fromList [("name", BE.BString "rename")]
              , BE.BDict $ Map.fromList [("name", BE.BString "update")]
              , BE.BDict $ Map.fromList [("name", BE.BString "delete")]
              , BE.BDict $ Map.fromList [("name", BE.BString "version")]
              ]
            )
          ]
      ]
    )
  , ("ops", BE.BDict $ Map.fromList [("shutdown", BE.BDict Map.empty)])
  ]

constructBencodeError :: PodRequestId -> BSLazy.ByteString -> BE.BEncode -> BE.BEncode
constructBencodeError requestId exceptionMessage exceptionData = BE.BDict $ Map.fromList
  [ ("id"        , BE.BString requestId)
  , ("ex-message", BE.BString exceptionMessage)
  , ("ex-data"   , exceptionData)
  , ("status", BE.BList [BE.BString "done", BE.BString "error"])
  ]

initialState :: IO PodState
initialState = do
  return PodState { _ekey = T.empty, _stashPath = "", _authenticated = False, _shutdown = False }

interactWithBencode :: IO ()
interactWithBencode = do
  s <- initialState
  interactWithBencode_ s

interactWithBencode_ :: PodState -> IO ()
interactWithBencode_ s = do
  contents <- BSLazy.getContents
  input    <- try $ evaluate $ BE.bRead contents
  let invalidBencode = BE.BList [BE.BString "error", BE.BString "Invalid bencode"]

  (newState, output) <- case (input :: Either SomeException (Maybe BE.BEncode)) of
    Left  e              -> return (s, invalidBencode)
    Right Nothing        -> return (s, invalidBencode)
    Right (Just bencode) -> handleRequest s bencode

  case output of
    BE.BList [BE.BString "error", BE.BString _] -> return ()
    x -> do
      let shutdown = _shutdown newState
      BSLazy.putStr $ BE.bPack x
      hFlush stdout

      if shutdown
        then return ()
        else do
          eof <- isEOF
          unless eof $ interactWithBencode_ newState
