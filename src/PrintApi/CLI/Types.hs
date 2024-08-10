{-# LANGUAGE CPP #-}

module PrintApi.CLI.Types
  ( Options (..)
  , parseOptions
  , runOptions
  , readCabalizedProcess
  , runCabalizedProcess
  , withInfo
  ) where

import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.List.Extra qualified as List
import Data.Version (showVersion)
import Options.Applicative
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

import Paths_print_api qualified
import PrintApi.CLI.Cmd.Dump (run)
import PrintApi.Utils

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
  stdOut <- readCabalizedProcess (Just TOOL_VERSION_ghc) "ghc" ["--print-libdir"]
  run (List.trimEnd $ ByteString.unpack stdOut) mModuleIgnoreList packageName

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc =
  info
    ( simpleVersioner (showVersion Paths_print_api.version)
        <*> helper
        <*> opts
    )
    $ progDesc desc

osPathOption :: ReadM OsPath
osPathOption = maybeReader OsPath.encodeUtf
