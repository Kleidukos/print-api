module Main where

import System.IO
import Options.Applicative

import DumpDecls.CLI.Types

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  parseResult <- execParser (parseOptions `withInfo` "Export the declarations of a Haskell package")
  runOptions parseResult
