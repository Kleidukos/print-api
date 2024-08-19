module Utils where

import Test.Tasty.HUnit
import System.Process.Typed
import Control.Monad.IO.Class

assertExitSuccess :: (MonadIO m) => String -> ExitCode -> m ()
assertExitSuccess _ ExitSuccess = pure ()
assertExitSuccess desc (ExitFailure n) = liftIO $ assertFailure $ desc <> ": Unexpected process failure (exit code " <> show n <> ")"
