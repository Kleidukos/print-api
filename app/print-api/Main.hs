{-# LANGUAGE CPP #-}

module Main where

import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.List.Extra qualified as List
import Options.Applicative
import PrintApi.Utils
import System.IO

import PrintApi.CLI.Cmd.Dump (run)
import PrintApi.CLI.Types

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  parseResult <- execParser (parseOptions `withInfo` "Export the declarations of a Haskell package")
  runOptions parseResult

runOptions
  :: Options
  -> IO ()
runOptions (Options packageName mIgnoreList usePublicOnly) = do
  stdOut <- readCabalizedProcess (Just TOOL_VERSION_ghc) "ghc" ["--print-libdir"]
  run (List.trimEnd $ ByteString.unpack stdOut) mIgnoreList usePublicOnly packageName
