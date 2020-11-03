module CommandParsers
  ( BrowseFormat(..)
  , Command(..)
  , commands
  )
where

import qualified Options.Applicative as O

data BrowseFormat = BrowseFormatTUI | BrowseFormatOrg
instance Show BrowseFormat where
  show BrowseFormatTUI = "tui"
  show BrowseFormatOrg = "org"

data Command = DumpCommand | BrowseCommand { format :: BrowseFormat }

browseFormatReader :: O.ReadM BrowseFormat
browseFormatReader = O.eitherReader f
 where
  f "tui" = Right BrowseFormatTUI
  f "org" = Right BrowseFormatOrg
  f x     = Left $ "Invalid browse format " ++ x

browseCommandParser :: O.Parser Command
browseCommandParser = BrowseCommand <$> O.option
  browseFormatReader
  (  O.long "format"
  <> O.short 'f'
  <> O.metavar "tui | org"
  <> O.help "Browse format"
  <> O.value BrowseFormatTUI
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
  [(["browse"], browseCommandParser, "Browse stash"), (["dump"], pure DumpCommand, "Dump stash")]
