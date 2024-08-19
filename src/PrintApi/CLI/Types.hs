module PrintApi.CLI.Types where

import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.List.Extra qualified as List
import Data.Version (showVersion)
import Options.Applicative
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath
import System.Process.Typed (ExitCode (..))
import System.Process.Typed qualified as Process

import Paths_print_api (version)
import PrintApi.CLI.Cmd.Dump (run)

data Options = Options
  { packageName :: String
  , moduleIgnoreList :: Maybe OsPath
  }
  deriving stock (Show, Eq)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> option
      str
      (long "package-name" <> short 'p' <> metavar "PACKAGE NAME" <> help "Name of the package")
    <*> optional
      (option osPathOption (long "modules-ignore-list" <> metavar "FILE" <> help "Read the file for a list of ignored modules (one per line)"))

runOptions
  :: Options
  -> IO ()
runOptions (Options packageName mModuleIgnoreList) = do
  (exitCode, stdOut, stdErr) <- Process.readProcess $ Process.shell "cabal exec -v0 -- ghc --print-libdir"
  case exitCode of
    ExitSuccess ->
      run (List.trimEnd $ ByteString.unpack stdOut) mModuleIgnoreList packageName
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

osPathOption :: ReadM OsPath
osPathOption = maybeReader OsPath.encodeUtf
