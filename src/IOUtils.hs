module IOUtils
  ( createStashDirectoryIfNotExists
  , edit
  , getEnvWithDefault
  , getEnvWithPromptFallback
  , getStashDirectory
  , readString
  , readUserResponseYesNo
  , readValidatedString
  , UserResponseYesNo(..)
  )
where
import Control.Exception
import Data.Maybe
import System.Environment (getEnv, setEnv)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.Console.Haskeline as HL
import qualified System.Directory as Directory
import qualified Text.Editor as TEditor

data UserResponseYesNo = URYes | URNo | URYesToAll | URNoToAll deriving (Eq, Show)

-- |Gets directory to use for storing stash files.
getStashDirectory :: IO String
getStashDirectory = do
  dir <- getEnvWithDefault "STASH_DIRECTORY" ".stash"
  Directory.makeAbsolute $ T.unpack dir

-- |Creates stash directory if it does not exist, and returns the stash directory path.
createStashDirectoryIfNotExists :: IO String
createStashDirectoryIfNotExists = do
  dir <- getStashDirectory
  Directory.createDirectoryIfMissing True dir
  return dir

-- |Reads a string from user by prompting for it.
readString :: String -> Bool -> IO T.Text
readString prompt mask = HL.runInputT HL.defaultSettings $ do
  let reader = if mask then HL.getPassword (Just '*') else HL.getInputLine
  line <- reader prompt
  return $ T.pack $ fromMaybe "" line

-- |Reads a non-empty string from user by prompting for it.
readNonEmptyString :: String -> Bool -> IO T.Text
readNonEmptyString prompt mask = do
  line <- readString prompt mask
  if T.null line
    then do
      putStrLn "🙀 Input cannot be empty."
      readNonEmptyString prompt mask
    else return line

-- |Reads a string from user satisfying a predicate function.
readValidatedString :: String -> Bool -> (T.Text -> IO Bool) -> IO T.Text
readValidatedString prompt mask validator = do
  line  <- readString prompt mask
  valid <- validator line
  if valid then return line else readValidatedString prompt mask validator

-- |Reads yes/no response from user.
readUserResponseYesNo :: String -> IO UserResponseYesNo
readUserResponseYesNo prompt = do
  let
    validtor x = do
      if x `elem` ["y", "yes", "n", "no", "yes-to-all", "no-to-all"]
        then return True
        else do
          putStrLn "Invalid response. Must be one of yes/y/no/n/yes-to-all/no-to-all."
          return False
  response <- readValidatedString (prompt ++ " (yes/y/no/yes-to-all/no-to-all): ") False validtor
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
getEnvWithPromptFallback name promptMessage mask confirm = do
  value <- try $ getEnv name
  case (value :: Either IOError String) of
    Left e -> do
      putStrLn $ "▸ Set " ++ name ++ " to avoid being prompted."
      key0 <- readNonEmptyString promptMessage mask
      if confirm
        then do
          putStrLn "Please confirm by entering again."
          key1 <- readNonEmptyString promptMessage mask
          if key0 == key1
            then do
              updateEnv name key0
            else do
              putStrLn "Entries do not match. Please try again."
              getEnvWithPromptFallback name promptMessage mask confirm
        else do
          updateEnv name key0
    Right key -> return $ T.pack key
 where
  updateEnv name key = do
    setEnv name $ T.unpack key
    return key

-- |Opens given value in an editor and returned the edited value.
--
-- By default the editor set it EDITOR environment variable is used. If it is not set
-- prompts user to enter editor command to use. If all fails, defaults to vim.
edit :: String -> T.Text -> IO T.Text
edit fileExtension initialContent = do
  let template = TEditor.mkTemplate fileExtension
  editorVar <- getEnvWithPromptFallback "EDITOR" "Enter editor path: " False False
  let editorCmdParts = T.words editorVar
  let editor = T.unpack $ if null editorCmdParts then "vim" else head editorCmdParts
  bytes <- TEditor.runSpecificEditor editor template $ TE.encodeUtf8 initialContent
  return $ TE.decodeUtf8 bytes
