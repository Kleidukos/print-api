module Main where

import Options.Applicative
import System.IO

import PrintApi.CLI.Types

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  parseResult <- execParser (parseOptions `withInfo` "Export the declarations of a Haskell package")
  runOptions parseResult
