module DumpDecls.CLI.Types where

import Data.Version (showVersion)
import Options.Applicative
import GHC.Paths (libdir)
import Paths_dump_decls (version)
import DumpDecls.CLI.Cmd.Dump (run)

data Options = Options
  { packageName :: String
  }
  deriving stock (Show, Eq)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> option
      auto
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
