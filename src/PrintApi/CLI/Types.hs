module PrintApi.CLI.Types where

import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.List.Extra qualified as List
import Data.Version (showVersion)
import Options.Applicative
import Paths_print_api (version)
import PrintApi.CLI.Cmd.Dump (run)
import System.Process.Typed (ExitCode (..))
import System.Process.Typed qualified as Process

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
  (exitCode, stdOut, stdErr) <- Process.readProcess $ Process.shell "cabal exec -v0 -- ghc --print-libdir"
  case exitCode of
    ExitSuccess ->
      run (List.trimEnd $ ByteString.unpack stdOut) packageName
    ExitFailure int -> do
      putStrLn $ "`cabal exec -v0 -- ghc --print-libdir` exited with error code " <> show int
      error $ ByteString.unpack stdErr

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc =
  info
    ( simpleVersioner (showVersion version)
        <*> helper
        <*> opts
    )
    $ progDesc desc
