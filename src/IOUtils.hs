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

import System.Console.Haskeline

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.Directory as Directory
import qualified Text.Editor as TEditor

data UserResponseYesNo = URYes | URNo | URYesToAll | URNoToAll deriving (Eq, Show)

getStashDirectory :: IO String
getStashDirectory = do
  dir <- getEnvWithDefault "STASH_DIRECTORY" ".stash"
  Directory.makeAbsolute dir

createStashDirectoryIfNotExists :: IO String
createStashDirectoryIfNotExists = do
  dir <- getStashDirectory
  Directory.createDirectoryIfMissing True dir
  return dir

readString :: String -> Bool -> IO String
readString prompt mask = runInputT defaultSettings $ do
  let reader = if mask then getPassword (Just '*') else getInputLine
  line <- reader prompt
  return $ fromMaybe "" line

readValidatedString :: String -> Bool -> (String -> IO Bool) -> IO String
readValidatedString prompt mask validator = do
  line  <- readString prompt mask
  valid <- validator line
  if valid then return line else readValidatedString prompt mask validator

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

getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault name defaultValue = do
  value <- try $ getEnv name
  case (value :: Either IOError String) of
    Left  e   -> return defaultValue
    Right key -> return key

getEnvWithPromptFallback :: String -> String -> Bool -> Bool -> IO String
getEnvWithPromptFallback name promptMessage mask confirm = do
  value <- try $ getEnv name
  case (value :: Either IOError String) of
    Left e -> do
      putStrLn $ "☠️  " ++ name ++ " not set."
      key0 <- readString promptMessage mask
      if confirm
        then do
          putStrLn $ "Please confirm again. " ++ promptMessage
          key1 <- readString promptMessage mask
          if key0 == key1
            then do
              updateEnv name key0
            else do
              putStrLn "Entries do not match. Please try again."
              getEnvWithPromptFallback name promptMessage mask confirm
        else do
          updateEnv name key0
    Right key -> return key
 where
  updateEnv name key = do
    setEnv name key
    return key

edit :: String -> T.Text -> IO T.Text
edit fileExtension initialContent = do
  let template = TEditor.mkTemplate fileExtension
  editorVar <- T.pack <$> getEnvWithPromptFallback "EDITOR" "Enter editor path: " False False
  let editorCmdParts = T.words editorVar
  let editor = T.unpack $ if null editorCmdParts then "vim" else head editorCmdParts
  bytes <- TEditor.runSpecificEditor editor template $ TE.encodeUtf8 initialContent
  return $ TE.decodeUtf8 bytes
