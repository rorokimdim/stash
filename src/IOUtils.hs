module IOUtils
  ( edit
  , getEnvWithDefault
  , getEnvWithPromptFallback
  , readString
  , readValidatedString
  )
where
import Control.Exception
import Data.Maybe
import System.Environment (getEnv, setEnv)

import System.Console.Haskeline

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Editor as TEditor

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

getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault name defaultValue = do
  value <- try $ getEnv name
  case (value :: Either IOError String) of
    Left  e   -> return defaultValue
    Right key -> return key

getEnvWithPromptFallback :: String -> String -> Bool -> IO String
getEnvWithPromptFallback name promptMessage mask = do
  value <- try $ getEnv name
  case (value :: Either IOError String) of
    Left e -> do
      putStrLn $ "☠️  " ++ name ++ " not set."
      key <- readString promptMessage mask
      setEnv name key
      return key
    Right key -> return key

edit :: String -> T.Text -> IO T.Text
edit fileExtension initialContent = do
  let template = TEditor.mkTemplate fileExtension
  editorVar <- T.pack <$> getEnvWithPromptFallback "EDITOR" "Enter editor path: " False
  let editorCmdParts = T.words editorVar
  let editor = T.unpack $ if null editorCmdParts then "vim" else head editorCmdParts
  bytes <- TEditor.runSpecificEditor editor template $ TE.encodeUtf8 initialContent
  return $ TE.decodeUtf8 bytes
