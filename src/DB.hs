module DB
  ( addNode
  , bootstrap
  , checkEncryptionKey
  , decryptNode
  , deleteNodes
  , doesDBExist
  , getAllNodes
  , getAllPlainNodes
  , getConfig
  , getIds
  , getIdsInPath
  , getKeys
  , getNodes
  , getPath
  , getPlainKeys
  , getPlainNodes
  , retrieve
  , save
  , setConfig
  , updateNode
  )
where

import System.FilePath.Posix (combine)
import Data.List (intercalate)
import Database.SQLite.Simple
import System.Directory (doesFileExist)
import Text.RawString.QQ

import qualified IOUtils
import qualified Cipher

import Types

import qualified Data.Text as T

instance FromRow Node where
  fromRow =
    Node <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow EncryptedKey where
  fromRow = field

instance FromRow NodeId where
  fromRow = field

getDBPath :: IO String
getDBPath = do
  dir <- IOUtils.getStashDirectory
  return $ combine dir "db"

getConnectionString :: IO String
getConnectionString = getDBPath

doesDBExist :: IO Bool
doesDBExist = do
  dbPath <- getDBPath
  doesFileExist dbPath

checkEncryptionKey :: EncryptionKey -> IO Bool
checkEncryptionKey ekey = do
  storedSalt <- getConfig "hashSalt"
  storedHash <- getConfig "encryptionKeyHash"
  return $ Cipher.hash storedSalt ekey == storedHash

bootstrap :: EncryptionKey -> IO ()
bootstrap ekey = do
  let splits  = T.splitOn ";;" bootstrapSQL
  let queries = [ Query x | x <- splits ]
  connectionString <- getConnectionString
  withConnection connectionString $ \conn -> withTransaction conn $ do
    mapM_ (execute_ conn) queries
    salt <- Cipher.generateHashSalt
    setConfig_ conn "hashSalt" salt
    setConfig_ conn "encryptionKeyHash" $ Cipher.hash salt ekey

clean :: PlainKey -> PlainValue -> (PlainKey, PlainValue)
clean k v = (T.strip k, T.strip v)

addNode :: EncryptionKey -> ParentId -> PlainKey -> PlainValue -> IO NodeId
addNode ekey pid key value = do
  let (cleanedKey, cleanedValue) = clean key value
  connectionString <- getConnectionString
  withConnection connectionString $ \conn -> addNode_ conn ekey pid cleanedKey cleanedValue

addNode_ :: Connection -> EncryptionKey -> ParentId -> PlainKey -> PlainValue -> IO NodeId
addNode_ conn ekey pid key value = do
  encryptedKey   <- Cipher.encrypt ekey key
  encryptedValue <- Cipher.encrypt ekey value
  salt           <- getHashSalt_ conn
  let hkey      = Cipher.hash salt key
  let hvalue    = Cipher.hash salt value
  let insertSQL = "INSERT INTO node (parent, hkey, hvalue, key, value) VALUES (?, ?, ?, ?, ?)"
  execute conn (Query insertSQL) (pid, hkey, hvalue, encryptedKey, encryptedValue)
  [Only nid] <-
    query conn "SELECT id FROM node WHERE parent=? AND hkey=?" (pid, hkey) :: IO [Only NodeId]
  return nid

save :: EncryptionKey -> [PlainKey] -> PlainValue -> IO [NodeId]
save ekey ks value = do
  connectionString <- getConnectionString
  withConnection connectionString $ \conn -> withTransaction conn $ save_ conn ekey 0 ks value

save_ :: Connection -> EncryptionKey -> ParentId -> [PlainKey] -> PlainValue -> IO [NodeId]
save_ conn ekey pid [k] value = do
  nid <- saveSingle conn ekey pid k value True
  return [nid]
save_ conn ekey pid (k : ks) value = do
  nextPid <- saveSingle conn ekey pid k T.empty False
  nids    <- save_ conn ekey nextPid ks value
  return $ nextPid : nids

saveSingle :: Connection -> EncryptionKey -> ParentId -> PlainKey -> PlainValue -> Bool -> IO NodeId
saveSingle conn ekey pid key value overwrite = do
  let (cleanedKey, cleanedValue) = clean key value
  encryptedKey   <- Cipher.encrypt ekey cleanedKey
  encryptedValue <- Cipher.encrypt ekey cleanedValue
  salt           <- getHashSalt_ conn
  let hkey   = Cipher.hash salt cleanedKey
  let hvalue = Cipher.hash salt cleanedValue
  let
    onConflictClause = if overwrite
      then "UPDATE SET hvalue=excluded.hvalue, value=excluded.value WHERE hvalue != excluded.hvalue"
      else "NOTHING"
  let
    insertSQL =
      T.pack
        $ "INSERT INTO node (parent, hkey, hvalue, key, value) VALUES (?, ?, ?, ?, ?) ON CONFLICT(hkey, parent) DO "
        ++ onConflictClause
  execute conn (Query insertSQL) (pid, hkey, hvalue, encryptedKey, encryptedValue)
  [Only nid] <-
    query conn "SELECT id FROM node WHERE parent=? AND hkey=?" (pid, hkey) :: IO [Only NodeId]
  return nid

updateNode :: EncryptionKey -> NodeId -> PlainKey -> PlainValue -> IO ()
updateNode ekey nid key value = do
  let (cleanedKey, cleanedValue) = clean key value
  connectionString <- getConnectionString
  withConnection connectionString $ \conn -> updateNode_ conn ekey nid cleanedKey cleanedValue

updateNode_ :: Connection -> EncryptionKey -> NodeId -> PlainKey -> PlainValue -> IO ()
updateNode_ conn ekey nid key value = do
  salt <- getHashSalt_ conn
  let hkey   = Cipher.hash salt key
  let hvalue = Cipher.hash salt value
  encryptedKey   <- Cipher.encrypt ekey key
  encryptedValue <- Cipher.encrypt ekey value
  let sql = "UPDATE node SET hkey=?, hvalue=?, key=?, value=? WHERE id=?"
  execute conn (Query sql) (hkey, hvalue, encryptedKey, encryptedValue, nid)

getPlainNodes :: EncryptionKey -> ParentId -> IO [PlainNode]
getPlainNodes ekey pid = do
  nodes <- getNodes pid
  mapM (decryptNode ekey) nodes

getPlainKeys :: EncryptionKey -> ParentId -> IO [PlainKey]
getPlainKeys ekey pid = do
  keys <- getKeys pid
  mapM (Cipher.decrypt ekey) keys

getNodes :: ParentId -> IO [Node]
getNodes pid = do
  connectionString <- getConnectionString
  withConnection connectionString $ \conn -> getNodes_ conn pid

getNodes_ :: Connection -> ParentId -> IO [Node]
getNodes_ conn pid = do
  query conn "SELECT * FROM node WHERE parent=?" (Only pid) :: IO [Node]

getAllPlainNodes :: EncryptionKey -> IO [PlainNode]
getAllPlainNodes ekey = do
  nodes <- getAllNodes
  mapM (decryptNode ekey) nodes

getAllNodes :: IO [Node]
getAllNodes = do
  connectionString <- getConnectionString
  withConnection connectionString $ \conn -> getAllNodes_ conn

getAllNodes_ :: Connection -> IO [Node]
getAllNodes_ conn = do
  query_ conn "SELECT * FROM node" :: IO [Node]

getKeys :: ParentId -> IO [EncryptedKey]
getKeys pid = do
  connectionString <- getConnectionString
  withConnection connectionString $ \conn -> getKeys_ conn pid

getKeys_ :: Connection -> ParentId -> IO [EncryptedKey]
getKeys_ conn pid = do
  query conn "SELECT key FROM node WHERE parent=?" (Only pid) :: IO [EncryptedKey]

getPath :: EncryptionKey -> NodeId -> IO [PlainKey]
getPath ekey nid = do
  connectionString <- getConnectionString
  withConnection connectionString $ \conn -> getPath_ conn ekey nid

getPath_ :: Connection -> EncryptionKey -> NodeId -> IO [PlainKey]
getPath_ conn ekey nid = do
  let
    sql = [r|
    WITH RECURSIVE f(parent, key) AS (
      SELECT
        parent,
        key
      FROM
        node
      WHERE
        id=?
      UNION ALL
      SELECT
        n.parent,
        n.key
      FROM
        node n
      INNER JOIN f ON n.id=f.parent
    )
    SELECT key FROM f
  |]
  keys <- query conn sql (Only nid) :: IO [EncryptedKey]
  mapM (Cipher.decrypt ekey) $ reverse keys

getIdsInPath :: [PlainKey] -> IO [NodeId]
getIdsInPath ks = do
  connectionString <- getConnectionString
  withConnection connectionString $ \conn -> getIdsInPath_ conn 0 ks

getIdsInPath_ :: Connection -> ParentId -> [PlainKey] -> IO [NodeId]
getIdsInPath_ _    _   []       = return []
getIdsInPath_ conn pid (k : ks) = do
  salt <- getHashSalt_ conn
  let hkey = Cipher.hash salt k
  result <- lookupId conn pid k
  case result of
    Just nid -> do
      cids <- getIdsInPath_ conn nid ks
      return $ nid : cids
    Nothing -> return []

getIds :: NodeId -> IO [NodeId]
getIds startNodeId = do
  connectionString <- getConnectionString
  withConnection connectionString $ \conn -> getIds_ conn startNodeId

getIds_ :: Connection -> NodeId -> IO [NodeId]
getIds_ conn startNodeId = do
  let
    sql = [r|
    WITH RECURSIVE f(id) AS (
      SELECT
        id
      FROM
        node
      WHERE
        id=?
      UNION ALL
      SELECT
        n.id
      FROM
        node n
      INNER JOIN f ON n.parent=f.id
    )
    SELECT id FROM f
  |]
  query conn sql (Only startNodeId) :: IO [NodeId]

deleteNodes :: [NodeId] -> IO ()
deleteNodes nids = do
  connectionString <- getConnectionString
  withConnection connectionString $ \conn -> deleteNodes_ conn nids

deleteNodes_ :: Connection -> [NodeId] -> IO ()
deleteNodes_ conn nids = do
  let sqlList = intercalate ", " $ map show nids
  let sql = T.concat ["DELETE FROM node WHERE id IN (", T.pack sqlList, ")"]
  execute_ conn (Query sql)

decryptNode :: EncryptionKey -> Node -> IO PlainNode
decryptNode ekey n = do
  plainKey   <- Cipher.decrypt ekey (_key n)
  plainValue <- Cipher.decrypt ekey (_value n)
  return PlainNode
    { __id       = _id n
    , __parent   = _parent n
    , __hkey     = _hkey n
    , __hvalue   = _hvalue n
    , __key      = plainKey
    , __value    = plainValue
    , __version  = _version n
    , __created  = _created n
    , __modified = _modified n
    }

retrieve :: EncryptionKey -> [PlainKey] -> IO (Maybe PlainValue)
retrieve ekey ks = do
  connectionString <- getConnectionString
  withConnection connectionString $ \conn -> retrieve_ conn ekey 0 ks

retrieve_ :: Connection -> EncryptionKey -> ParentId -> [PlainKey] -> IO (Maybe PlainValue)
retrieve_ conn ekey pid [k] = do
  result <- lookupId conn pid k
  case result of
    Just nid -> getValueById conn ekey nid
    Nothing  -> return Nothing
retrieve_ conn ekey pid (k : ks) = do
  result <- lookupId conn pid k
  case result of
    Just nid -> retrieve_ conn ekey nid ks
    Nothing  -> return Nothing

lookupId :: Connection -> ParentId -> PlainKey -> IO (Maybe NodeId)
lookupId conn pid k = do
  salt <- getHashSalt_ conn
  let hkey = Cipher.hash salt k
  result <-
    query conn "SELECT id FROM node WHERE parent=? AND hkey=?" (pid, hkey) :: IO [Only NodeId]
  case result of
    [Only nid] -> return $ Just nid
    []         -> return Nothing

getValueById :: Connection -> EncryptionKey -> NodeId -> IO (Maybe PlainValue)
getValueById conn ekey nid = do
  [Only result] <-
    query conn "SELECT value FROM node WHERE id=?" (Only nid) :: IO [Only (Maybe EncryptedValue)]
  case result of
    Just encryptedValue -> do
      value <- Cipher.decrypt ekey encryptedValue
      return (Just value)
    Nothing -> return Nothing

getConfig :: T.Text -> IO T.Text
getConfig name = do
  connectionString <- getConnectionString
  withConnection connectionString $ \conn -> getConfig_ conn name

getConfig_ :: Connection -> T.Text -> IO T.Text
getConfig_ conn name = do
  result <- query conn "SELECT value FROM config WHERE name=?" (Only name) :: IO [Only T.Text]
  case result of
    [Only value] -> return value
    []           -> return ""

setConfig :: T.Text -> T.Text -> IO ()
setConfig name value = do
  connectionString <- getConnectionString
  withConnection connectionString $ \conn -> setConfig_ conn name value

setConfig_ :: Connection -> T.Text -> T.Text -> IO ()
setConfig_ conn name value = do
  let
    insertSQL
      = "INSERT INTO config (name, value) VALUES (?, ?) ON CONFLICT(name) DO UPDATE SET value=excluded.value"
  execute conn (Query $ T.pack insertSQL) (name, value)

getHashSalt :: IO HashSalt
getHashSalt = do
  connectionString <- getConnectionString
  withConnection connectionString getHashSalt_

getHashSalt_ :: Connection -> IO HashSalt
getHashSalt_ conn = getConfig_ conn "hashSalt"

bootstrapSQL :: T.Text
bootstrapSQL = [r|
CREATE TABLE IF NOT EXISTS config (
  name TEXT NOT NULL,
  value TEXT NOT NULL
);;
CREATE UNIQUE INDEX IF NOT EXISTS ids_config_name ON config(name);;

CREATE TABLE IF NOT EXISTS node (
  id INTEGER PRIMARY KEY,
  parent INTEGER DEFAULT 0,
  hkey TEXT NOT NULL,
  hvalue TEXT NOT NULL,
  key BLOB NOT NULL,
  value BLOB,
  version INTEGER DEFAULT 1,
  created TEXT DEFAULT CURRENT_TIMESTAMP,
  modified TEXT DEFAULT CURRENT_TIMESTAMP
);;
CREATE UNIQUE INDEX IF NOT EXISTS idx_node_hkey_parent ON node(hkey, parent);;
CREATE INDEX IF NOT EXISTS idx_node_parent ON node(parent);;

CREATE TABLE IF NOT EXISTS node_history (
  id INTEGER NOT NULL,
  parent INTEGER,
  hkey TEXT NOT NULL,
  hvalue TEXT NOT NULL,
  key BLOB NOT NULL,
  value BLOB NOT NULL,
  version INTEGER NOT NULL,
  created TEXT NOT NULL,
  modified TEXT NOT NULL
);;
CREATE INDEX IF NOT EXISTS idx_node_history_id_modified ON node(id, modified);;

CREATE TRIGGER IF NOT EXISTS trigger_node_on_update
AFTER UPDATE OF parent, hkey, hvalue ON node FOR EACH ROW
BEGIN
  UPDATE node
    SET modified = CURRENT_TIMESTAMP,
        created = OLD.created,
        version = OLD.version + 1
  WHERE id=NEW.id;

  INSERT INTO node_history (id, parent, hkey, hvalue, key, value, version, created, modified) VALUES (
    OLD.id,
    OLD.parent,
    OLD.hkey,
    OLD.hvalue,
    OLD.key,
    OLD.value,
    OLD.version,
    OLD.created,
    OLD.modified
  );

  DELETE FROM node_history WHERE
    id=NEW.id AND
    version <= NEW.version - 10;
END
|]
