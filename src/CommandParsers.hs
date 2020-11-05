module CommandParsers
  ( BrowseFormat(..)
  , Command(..)
  , DumpFormat(..)
  , commands
  )
where

import qualified Options.Applicative as O

data BrowseFormat = BrowseFormatMarkdown | BrowseFormatOrg | BrowseFormatTUI
instance Show BrowseFormat where
  show BrowseFormatMarkdown = "markdown"
  show BrowseFormatOrg      = "org"
  show BrowseFormatTUI      = "tui"

data DumpFormat = DumpFormatMarkdown | DumpFormatOrg
instance Show DumpFormat where
  show DumpFormatMarkdown = "markdown"
  show DumpFormatOrg      = "org"

data Command = DumpCommand DumpFormat | BrowseCommand BrowseFormat

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
browseCommandParser = BrowseCommand <$> O.option
  browseFormatReader
  (  O.long "format"
  <> O.short 'f'
  <> O.metavar "markdown | org | tui"
  <> O.help "Browse format"
  <> O.value BrowseFormatTUI
  <> O.showDefault
  )

dumpCommandParser :: O.Parser Command
dumpCommandParser = DumpCommand <$> O.option
  dumpFormatReader
  (  O.long "format"
  <> O.short 'f'
  <> O.metavar "markdown | org"
  <> O.help "Dump format"
  <> O.value DumpFormatOrg
  <> O.showDefault
  )

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
  [(["browse"], browseCommandParser, "Browse stash"), (["dump"], dumpCommandParser, "Dump stash")]
