module CommandParsers
  ( BrowseFormat(..)
  , Command(..)
  , DumpFormat(..)
  , commands
  )
where

import Data.List (isPrefixOf, isSuffixOf)
import Data.String (IsString)
import System.FilePath.Posix
  (addTrailingPathSeparator, combine, hasTrailingPathSeparator, takeDirectory, takeFileName)

import qualified Options.Applicative as O
import qualified System.Directory as Directory

import qualified IOUtils

data BrowseFormat = BrowseFormatMarkdown | BrowseFormatOrg | BrowseFormatTUI
instance Show BrowseFormat where
  show BrowseFormatMarkdown = "markdown"
  show BrowseFormatOrg      = "org"
  show BrowseFormatTUI      = "tui"

data DumpFormat = DumpFormatMarkdown | DumpFormatOrg
instance Show DumpFormat where
  show DumpFormatMarkdown = "markdown"
  show DumpFormatOrg      = "org"

data Command = DumpCommand FilePath DumpFormat
             | BrowseCommand FilePath BrowseFormat
             | BackupCommand FilePath
             | CreateCommand FilePath

browseFormatReader :: O.ReadM BrowseFormat
browseFormatReader = O.eitherReader f
 where
  f "md"       = Right BrowseFormatMarkdown
  f "markdown" = Right BrowseFormatMarkdown
  f "org"      = Right BrowseFormatOrg
  f "tui"      = Right BrowseFormatTUI
  f x          = Left $ "Invalid browse format " ++ x

dumpFormatReader :: O.ReadM DumpFormat
dumpFormatReader = O.eitherReader f
 where
  f "md"       = Right DumpFormatMarkdown
  f "markdown" = Right DumpFormatMarkdown
  f "org"      = Right DumpFormatOrg
  f x          = Left $ "Invalid dump format " ++ x

browseCommandParser :: O.Parser Command
browseCommandParser = BrowseCommand <$> stashFilePathArgument <*> O.option
  browseFormatReader
  (  O.long "format"
  <> O.short 'f'
  <> O.metavar "markdown | org | tui"
  <> O.help "Browse format"
  <> O.value BrowseFormatTUI
  <> O.completeWith ["markdown", "org", "tui"]
  <> O.showDefault
  )

dumpCommandParser :: O.Parser Command
dumpCommandParser = DumpCommand <$> stashFilePathArgument <*> O.option
  dumpFormatReader
  (  O.long "format"
  <> O.short 'f'
  <> O.metavar "markdown | org"
  <> O.help "Dump format"
  <> O.value DumpFormatOrg
  <> O.completeWith ["markdown", "org"]
  <> O.showDefault
  )

stashFilePathComplete :: String -> IO [String]
stashFilePathComplete prefix = do
  let
    directoryPath = takeDirectory prefix
    namePrefix    = takeFileName prefix
    simplifyPath ('.' : '/' : xs) | not ("./" `isPrefixOf` prefix) = xs
    simplifyPath xs = xs

  normalizedDirectoryPath <- IOUtils.normalizePath directoryPath
  directoryExists <- Directory.doesDirectoryExist normalizedDirectoryPath
  paths <- if directoryExists then Directory.listDirectory normalizedDirectoryPath else return []

  let
    filteredPaths =
      [ simplifyPath (combine directoryPath p)
      | p <- paths
      , ".stash" `isSuffixOf` p
      , namePrefix `isPrefixOf` p
      ]
  if null filteredPaths && not (hasTrailingPathSeparator prefix) && directoryExists
    then stashFilePathComplete (addTrailingPathSeparator prefix)
    else return filteredPaths


stashFilePathArgument :: IsString s => O.Parser s
stashFilePathArgument = O.strArgument
  (  O.metavar "FILE"
  <> O.action "directory"
  <> O.completer (O.mkCompleter stashFilePathComplete)
  <> O.help "Path to stash file"
  )

backupCommandParser :: O.Parser Command
backupCommandParser = BackupCommand <$> stashFilePathArgument

createCommandParser :: O.Parser Command
createCommandParser = CreateCommand <$> stashFilePathArgument

type CommandAlias = String
type CommandDescription = String
buildParser
  :: [([CommandAlias], O.Parser Command, CommandDescription)] -> [O.Mod O.CommandFields Command]
buildParser xs = concat $ do
  (aliases, parser, description) <- xs
  let
    ps = do
      alias <- aliases
      let options = parser O.<**> O.helper
      return $ O.command alias $ O.info options $ O.progDesc description
  return ps

commands :: [O.Mod O.CommandFields Command]
commands = buildParser
  [ (["backup"], backupCommandParser, "Backup stash")
  , (["browse"], browseCommandParser, "Browse stash")
  , (["dump"]  , dumpCommandParser  , "Dump stash")
  , (["create"], createCommandParser, "Creates stash database")
  ]
