module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Char (isSpace)
import Options.Applicative
import PrintApi.CLI.Types
import PrintApi.Utils (readCabalizedProcess, runCabalizedProcess)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  -- We parse the command line arguments here in order to detect flags like
  -- @--help@ or @--version@.
  _ <- execParser (parseOptions `withInfo` "Export the declarations of a Haskell package")
  ghcVersion <- getGhcVersion
  hPutStrLn stderr $ "Detected GHC version: " <> ghcVersion
  runCabalizedProcess (Just ghcVersion) ("print-api-" <> ghcVersion) args

getGhcVersion :: IO String
getGhcVersion =
  ByteString.unpack . ByteString.dropWhileEnd isSpace
    <$> readCabalizedProcess Nothing "ghc" ["--numeric-version"]
