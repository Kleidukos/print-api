-- |
--  Module      : PrintApi.CLI.Types
--  Copyright   : © Hécate, 2024
--  License     : MIT
--  Maintainer  : hecate@glitchbra.in
--  Visibility  : Public
--
--  The option parsing for the CLI
module PrintApi.CLI.Types
  ( Options (..)
  , RunMode (..)
  , parseOptions
  , withInfo
  ) where

import Data.Version (showVersion)
import Options.Applicative
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath

import Paths_print_api qualified

data Options = Options
  { packageName :: String
  , ignoreList :: Maybe OsPath
  , haddockMetadata :: Bool
  }
  deriving stock (Show, Ord, Eq)

data RunMode
  = IgnoreList OsPath
  | HaddockMetadata
  deriving stock (Show, Ord, Eq)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> option
      str
      (long "package-name" <> short 'p' <> metavar "PACKAGE NAME" <> help "Name of the package")
    <*> optional
      (option osPathOption (long "modules-ignore-list" <> metavar "FILE" <> help "Read the file for a list of ignored modules (one per line)"))
    <*> switch (long "public-only")

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
