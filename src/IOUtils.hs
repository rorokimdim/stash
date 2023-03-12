module IOUtils
  ( createMissingDirectories
  , getEnvWithDefault
  , getEnvWithPromptFallback
  , getEnvWithPromptFallback_
  , logTime
  , normalizePath
  , readNonEmptyString
  , readNonEmptyString_
  , readString
  , readString_
  , readUserResponseYesNo
  , readUserResponseYesNo_
  , readValidatedString
  , readValidatedString_
  , UserResponseYesNo(..)
  ) where
import Control.Exception (try)
import Data.Maybe (fromMaybe)
import Data.Time (diffUTCTime, getCurrentTime)
import System.CPUTime (getCPUTime)
import System.Environment (getEnv, setEnv)
import System.FilePath.Posix (pathSeparator, takeDirectory)
import System.IO (Handle, stdin)
import Text.Printf (printf)

import qualified Control.Logging as L
import qualified Data.Text as T
import qualified System.Console.Haskeline as HL
import qualified System.Directory as Directory

data UserResponseYesNo = URYes | URNo | URYesToAll | URNoToAll deriving (Eq, Show)

-- |Gets normalized file/directory path.
normalizePath :: String -> IO String
normalizePath ('~' : xs) = do
  home <- Directory.getHomeDirectory
  Directory.makeAbsolute $ home <> [pathSeparator] <> xs
normalizePath p = Directory.makeAbsolute p

-- |Creates missing directories in given path.
createMissingDirectories :: FilePath -> IO ()
createMissingDirectories path = do
  dir <- takeDirectory <$> normalizePath path
  Directory.createDirectoryIfMissing True dir

-- |Reads a string from user by prompting for it.
readString :: String -> Bool -> IO T.Text
readString prompt mask = readString_ stdin prompt mask

-- |Reads a string from a file handle.
readString_ :: Handle -> String -> Bool -> IO T.Text
readString_ handle prompt mask = HL.runInputTBehavior (HL.useFileHandle handle) HL.defaultSettings $ do
  let reader = if mask then HL.getPassword (Just '*') else HL.getInputLine
  line <- reader prompt
  return $ T.pack $ fromMaybe "" line

-- |Reads a non-empty string from user by prompting for it.
readNonEmptyString :: String -> Bool -> IO T.Text
readNonEmptyString prompt mask = readNonEmptyString_ stdin prompt mask

-- |Reads a non-empty string from a file handle.
readNonEmptyString_ :: Handle -> String -> Bool -> IO T.Text
readNonEmptyString_ handle prompt mask = do
  line <- readString_ handle prompt mask
  if T.null line
    then do
      putStrLn "🙀 Input cannot be empty."
      readNonEmptyString_ handle prompt mask
    else return line

-- |Reads a string from user satisfying a predicate function.
readValidatedString :: String -> Bool -> (T.Text -> IO Bool) -> IO T.Text
readValidatedString prompt mask validator = readValidatedString_ stdin prompt mask validator

-- |Reads a string from a file handle, satisfying a predicate function.
readValidatedString_ :: Handle -> String -> Bool -> (T.Text -> IO Bool) -> IO T.Text
readValidatedString_ handle prompt mask validator = do
  line  <- readString_ handle prompt mask
  valid <- validator line
  if valid then return line else readValidatedString_ handle prompt mask validator

-- |Reads yes/no response from user.
readUserResponseYesNo :: String -> IO UserResponseYesNo
readUserResponseYesNo prompt = readUserResponseYesNo_ stdin prompt

-- |Reads yes/no response from a file handle.
readUserResponseYesNo_ :: Handle -> String -> IO UserResponseYesNo
readUserResponseYesNo_ handle prompt = do
  let
    validtor x = do
      if x `elem` ["y", "yes", "n", "no", "yes-to-all", "no-to-all"]
        then return True
        else do
          putStrLn "Invalid response. Must be one of yes/y/no/n/yes-to-all/no-to-all."
          return False
  response <- readValidatedString_ handle (prompt ++ " (yes/y/no/yes-to-all/no-to-all): ") False validtor
  return $ case response of
    "y"          -> URYes
    "yes"        -> URYes
    "n"          -> URNo
    "no"         -> URNo
    "yes-to-all" -> URYesToAll
    "no-to-all"  -> URNoToAll

-- |Gets value of an environment variable, returning provided default-value if it is not set.
getEnvWithDefault :: String -> T.Text -> IO T.Text
getEnvWithDefault name defaultValue = do
  value <- try $ getEnv name
  case (value :: Either IOError String) of
    Left  e   -> return defaultValue
    Right key -> return $ T.pack key

-- |Gets value of an environment variable, and if it is not set, prompts for it.
getEnvWithPromptFallback :: String -> String -> Bool -> Bool -> IO T.Text
getEnvWithPromptFallback name promptMessage mask confirm =
  getEnvWithPromptFallback_ stdin name promptMessage mask confirm

-- |Gets value of an environment variable, and if it is not set, gets it a file handle.
getEnvWithPromptFallback_ :: Handle -> String -> String -> Bool -> Bool -> IO T.Text
getEnvWithPromptFallback_ handle name promptMessage mask confirm = do
  value <- try $ getEnv name
  case (value :: Either IOError String) of
    Left e -> do
      key0 <- readNonEmptyString_ handle promptMessage mask
      if confirm
        then do
          putStrLn "Please confirm by entering again."
          key1 <- readNonEmptyString_ handle promptMessage mask
          if key0 == key1
            then do
              updateEnv name key0
            else do
              putStrLn "Entries do not match. Please try again."
              getEnvWithPromptFallback_ handle name promptMessage mask confirm
        else do
          updateEnv name key0
    Right key -> return $ T.pack key
 where
  updateEnv name key = do
    setEnv name $ T.unpack key
    return key

-- |Runs an IO action and logs timing information.
logTime :: T.Text -> IO a -> IO a
logTime message ioa = L.withStderrLogging $ do
  startCPUTime <- getCPUTime
  startTime    <- getCurrentTime
  a            <- ioa
  endCPUtime   <- getCPUTime
  endTime      <- getCurrentTime

  let
    cpuDurationMS :: Double
    cpuDurationMS = fromIntegral (endCPUtime - startCPUTime) * 1e-9

    durationMS :: Double
    durationMS = 1000 * realToFrac (diffUTCTime endTime startTime)

  L.debug'
    $  message
    <> " [clock="
    <> T.pack (printf "%.6fms" durationMS)
    <> ", "
    <> "cpu="
    <> T.pack (printf "%.6fms" cpuDurationMS)
    <> "]"
  return a
