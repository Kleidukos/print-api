module DumpDecls.CLI.Types where

import Data.Version (showVersion)
import DumpDecls.CLI.Cmd.Dump (run)
import GHC.Paths (libdir)
import Options.Applicative
import Paths_dump_decls (version)

data Options = Options
  { packageName :: String
  }
  deriving stock (Show, Eq)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> option
      str
      (long "package-name" <> short 'p' <> metavar "PACKAGE NAME" <> help "Name of the package")

runOptions
  :: Options
  -> IO ()
runOptions (Options packageName) = do
  run libdir packageName

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc =
  info
    ( simpleVersioner (showVersion version)
        <*> helper
        <*> opts
    )
    $ progDesc desc
