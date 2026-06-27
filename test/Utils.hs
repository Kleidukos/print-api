module Utils where

import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.List.Extra qualified as List
import System.Process.Typed
import Test.Tasty.HUnit

assertExitSuccess :: (MonadIO m) => String -> ExitCode -> m ()
assertExitSuccess _ ExitSuccess = pure ()
assertExitSuccess desc (ExitFailure n) = liftIO $ assertFailure $ desc <> ": Unexpected process failure (exit code " <> show n <> ")"

-- | Resolve GHC's library directory via cabal, the way @print-api@ does at runtime.
getLibdir :: (MonadIO m) => m FilePath
getLibdir = do
  (exitCode, stdOut, _stdErr) <- readProcess $ shell "cabal exec -v0 -- ghc --print-libdir"
  assertExitSuccess "`cabal exec -v0 -- ghc --print-libdir`" exitCode
  pure $ List.trimEnd $ C8.unpack stdOut
