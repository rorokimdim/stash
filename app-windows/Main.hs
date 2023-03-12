--
-- A minimal main app without brick/vty.
--
module Main where

import Control.Monad (join, unless, void, when)
import Control.Monad.Trans (liftIO)
import Data.List (findIndex, sortBy)
import Data.Maybe (fromMaybe)
import System.Directory (copyFileWithMetadata, doesDirectoryExist)
import System.Exit (die)
import System.FilePath.Posix (combine, takeBaseName, takeDirectory, takeFileName)
import System.IO (Handle, IOMode(ReadMode), hClose, hFlush, hIsTerminalDevice, openFile, stdin, stdout)

import qualified Control.Logging as L
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.Algorithm.Diff as Diff
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as Set
import qualified Data.IORef as IORef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time as Time
import qualified Data.Vector as Vec
import qualified Options.Applicative as O
import qualified System.IO.Memoize as Memoize
import qualified Text.Fuzzy as TF
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Text.Tabl as Table

import qualified BabashkaPod as BPod
import qualified Cipher
import qualified CommandParsers as C
import qualified DB
import qualified IOUtils
import qualified TextTransform
import qualified Version

import Types

-- |Gets encryption key from environment or user and memoizes it for subsequent calls.
getEncryptionKey :: IO EncryptionKey
getEncryptionKey = join $ Memoize.once $ getEncryptionKey_ stdin

getEncryptionKey_ :: Handle -> IO EncryptionKey
getEncryptionKey_ handle = do
  dbPath <- DB.getDBPath
  ekey   <- IOUtils.getEnvWithPromptFallback_
    handle
    "STASH_ENCRYPTION_KEY"
    ("Enter encryption key for " <> takeFileName dbPath <> ": ")
    True
    False

  valid <- DB.checkEncryptionKey ekey
  if valid then return ekey else die $ "\n☠️  Encryption key is invalid for stash file at " <> dbPath <> "."

getEncryptionKeyWithConfirmation :: IO EncryptionKey
getEncryptionKeyWithConfirmation = do
  ekey <- IOUtils.getEnvWithPromptFallback "STASH_ENCRYPTION_KEY" "Enter encryption key: " True True
  unless (T.length ekey <= Cipher.maxEncryptionKeyLength)
    $  die
    $  "☠️  Sorry, encryption key can currently be upto "
    ++ show Cipher.maxEncryptionKeyLength
    ++ " characters."
  return ekey

printDiff :: T.Text -> T.Text -> IO ()
printDiff t1 t2 = do
  let diffs = Diff.getGroupedDiff (T.lines t1) (T.lines t2)

  let
    toDoc xs = PP.text $ T.unpack $ T.unlines xs
    pdiff ((Diff.Both xs _) : ys) = do
      if length xs <= 2 then PP.putDoc $ toDoc xs else TIO.putStrLn "..."
      pdiff ys
    pdiff ((Diff.First xs) : ys) = do
      PP.putDoc $ PP.red $ "-" <> toDoc xs <> PP.linebreak
      pdiff ys
    pdiff ((Diff.Second xs) : ys) = do
      PP.putDoc $ PP.green $ "+" <> toDoc xs <> PP.linebreak
      pdiff ys
    pdiff [] = return ()

  pdiff diffs

dump :: TextFormat -> IO ()
dump JSONText = do
  ekey      <- getEncryptionKey
  plainTree <- DB.getPlainTree ekey 0
  let
    config = AesonPretty.Config
      { confIndent          = AesonPretty.Spaces 2
      , confCompare         = mempty
      , confNumFormat       = AesonPretty.Generic
      , confTrailingNewline = False
      }
  BLC.putStrLn $ AesonPretty.encodePretty' config plainTree
dump format = do
  ekey       <- getEncryptionKey
  plainNodes <- DB.getAllPlainNodes ekey

  text       <- IOUtils.logTime "toText" $ pure $ TextTransform.toText format plainNodes
  TIO.putStrLn ""
  TIO.putStrLn text

backup :: IO ()
backup = do
  source   <- DB.getDBPath
  userTime <- Time.getZonedTime

  let
    destinationDirectory = takeDirectory source
    destinationFileName  = "backup-" <> takeBaseName source <> "-" <> filter (/= ' ') (show userTime) <> ".stash"
    destination          = combine destinationDirectory destinationFileName

  copyFileWithMetadata source destination
  putStrLn $ "Backed up " <> source <> " to " <> destination

ingestTextInteractively :: EncryptionKey -> TextFormat -> T.Text -> IO [NodeId]
ingestTextInteractively ekey format text = ingestTextInteractively_ stdin ekey format text

ingestTextInteractively_ :: Handle -> EncryptionKey -> TextFormat -> T.Text -> IO [NodeId]
ingestTextInteractively_ handle ekey format text = do
  lastUserResponseRef <- IORef.newIORef IOUtils.URNo

  let
    walker :: [NodeId] -> [PlainKey] -> PlainValue -> IO [NodeId]
    walker nids ks body = do
      oldValue <- DB.retrieve ekey ks
      case oldValue of
        Nothing      -> DB.save ekey ks body
        Just ""      -> DB.save ekey ks body
        Just oldBody -> do
          if oldBody /= body
            then do
              lastUserResponse <- IORef.readIORef lastUserResponseRef
              case lastUserResponse of
                IOUtils.URYesToAll -> do
                  ids <- DB.save ekey ks body
                  return $ ids ++ nids
                IOUtils.URNoToAll -> return nids
                _                 -> do
                  TIO.putStrLn $ T.concat ["▸ Value of [", T.intercalate " > " ks, "] differ:"]
                  printDiff oldBody body

                  userResponse <- IOUtils.readUserResponseYesNo_ handle "Accept this change?"
                  IORef.writeIORef lastUserResponseRef userResponse

                  let
                    handleUserResponse r | r `elem` [IOUtils.URYes, IOUtils.URYesToAll] = do
                      ids <- DB.save ekey ks body
                      return $ ids ++ nids
                    handleUserResponse r | r `elem` [IOUtils.URNo, IOUtils.URNoToAll] = do
                      ids <- DB.getIdsInPath ks
                      return $ ids ++ nids

                  handleUserResponse userResponse
            else return nids

  TextTransform.walkText [] format text walker

importText :: TextFormat -> IO ()
importText format = do
  isTerminal   <- hIsTerminalDevice stdin
  tty          <- openFile "/dev/tty" ReadMode
  (ekey, text) <- if isTerminal
    then do
      ekey <- getEncryptionKey
      putStrLn ""
      hFlush stdout
      putStrLn $ "→ Please enter/paste text in '" <> show format <> "' and press ctrl-d when done:"
      text <- TIO.getContents
      return (ekey, text)
    else do
      putStrLn
        "Waiting for piped input... if this is stuck it probably means the piped command is waiting for you to enter something..."
      text <- TIO.getContents
      ekey <- getEncryptionKey_ tty
      putStrLn ""
      hFlush stdout
      return (ekey, text)

  ingestTextInteractively_ tty ekey format text
  hClose tty

initialize :: FilePath -> Bool -> IO ()
initialize path createIfMissing = do
  dbPath           <- DB.setDBPath path
  validationResult <- DB.validateDBPath dbPath
  case validationResult of
    DB.NonExistentDBFile -> do
      unless createIfMissing $ die $ "☠️  stash file " <> dbPath <> " does not exist."
      isDirectory <- doesDirectoryExist dbPath
      when isDirectory $ die $ "☠️  " <> dbPath <> " is a directory. Did you mean " <> dbPath <> ".stash?"
      TIO.putStrLn $ "Creating new stash file " <> T.pack dbPath <> "..."
      ekey <- getEncryptionKeyWithConfirmation
      IOUtils.createMissingDirectories dbPath
      DB.bootstrap ekey
      TIO.putStrLn $ T.intercalate
        "\n"
        [ T.append "Created stash file " $ T.pack dbPath
        , "\nOnly a salted hash (good random salt + SHA512) of the encryption-key was saved."
        , "Stash will prompt for the encryption-key when needed."
        , "(unless STASH_ENCRYPTION_KEY environment variable is set)"
        ]
    DB.InvalidDBFile -> die $ "☠️  Invalid stash file " <> dbPath
    DB.ValidDBFile   -> return ()

processCommand :: C.Command -> IO ()
processCommand (C.CreateCommand dbPath) = initialize dbPath True
processCommand (C.BackupCommand dbPath) = do
  initialize dbPath False
  backup
processCommand (C.BrowseCommand dbPath format) = do
  putStrLn "Browse command not supported."
processCommand (C.DumpCommand dbPath format) = do
  initialize dbPath False
  case format of
    C.DumpFormatJSON     -> dump JSONText
    C.DumpFormatMarkdown -> dump MarkdownText
    C.DumpFormatOrg      -> dump OrgText
processCommand (C.ImportCommand dbPath format) = do
  initialize dbPath False
  case format of
    C.ImportFormatMarkdown -> importText MarkdownText
    C.ImportFormatOrg      -> importText OrgText
processCommand C.VersionCommand = putStrLn Version.appVersion

setUpLogging :: IO ()
setUpLogging = do
  logLevel <- IOUtils.getEnvWithDefault "STASH_LOG_LEVEL" "INFO"
  case logLevel of
    "DEBUG" -> L.setLogLevel L.LevelDebug
    "INFO"  -> L.setLogLevel L.LevelInfo
    "WARN"  -> L.setLogLevel L.LevelWarn
    _       -> L.setLogLevel L.LevelError

main :: IO ()
main = do
  setUpLogging
  pod <- IOUtils.getEnvWithDefault "BABASHKA_POD" "false"
  case pod of
    "true"  -> BPod.interactWithBencode
    "false" -> do
      cmd <- O.customExecParser preferences opts
      processCommand cmd
 where
  parser = O.subparser $ mconcat C.commands
  opts   = O.info
    (parser O.<**> O.helper)
    (O.fullDesc <> O.progDesc "stash [create | browse | dump | backup | import]" <> O.header
      ("Stash " <> Version.appVersion <> " https://github.com/rorokimdim/stash")
    )
  preferences = O.prefs (O.showHelpOnError <> O.showHelpOnEmpty)
