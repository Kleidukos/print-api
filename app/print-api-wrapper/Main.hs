module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Char (isSpace)
import PrintApi.CLI.Types (readCabalizedProcess, runCabalizedProcess)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  ghcVersion <- getGhcVersion
  hPutStrLn stderr $ "Detected GHC version: " <> ghcVersion
  runCabalizedProcess (Just ghcVersion) ("print-api-" <> ghcVersion) args

getGhcVersion :: IO String
getGhcVersion =
  ByteString.unpack . ByteString.dropWhileEnd isSpace
    <$> readCabalizedProcess Nothing "ghc" ["--numeric-version"]
