{-# LANGUAGE LambdaCase #-}

module PrintApi.Utils
  ( readCabalizedProcess
  , runCabalizedProcess
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Functor ((<&>))
import System.Environment (lookupEnv)
import System.Process qualified as Process (CmdSpec (..), showCommandForUser)
import System.Process.Typed (ExitCode (..))
import System.Process.Typed qualified as Process
import System.Process.Typed.Internal (ProcessConfig (pcCmdSpec))

readCabalizedProcess
  :: Maybe String
  -- ^ The GHC version to use
  -> String
  -> [String]
  -> IO ByteString
readCabalizedProcess = cabalizedProcessHelper Process.readProcess

runCabalizedProcess
  :: Maybe String
  -- ^ The GHC version to use
  -> String
  -> [String]
  -> IO ()
runCabalizedProcess =
  cabalizedProcessHelper
    (fmap adjust . Process.readProcessStderr)
  where
    adjust (exitCode, stdErr) = (exitCode, (), stdErr)

cabalizedProcessHelper
  :: (ProcessConfig () () () -> IO (ExitCode, stdout, ByteString))
  -> Maybe String
  -- ^ The GHC version to use
  -> String
  -> [String]
  -> IO stdout
cabalizedProcessHelper go ghcVersion exe args = do
  pc <- cabalizedProc ghcVersion exe args
  (exitCode, stdOut, stdErr) <- go pc
  case exitCode of
    ExitSuccess -> pure stdOut
    ExitFailure int -> do
      putStrLn $ "`" <> showCommandForUser pc <> "` exited with error code " <> show int
      error $ ByteString.unpack stdErr

cabalizedProc
  :: Maybe String
  -- ^ The GHC version to use
  -> String
  -> [String]
  -> IO (ProcessConfig () () ())
cabalizedProc ghcVersion exe args =
  -- If there is a GHC_ENVIRONMENT environment variable set, assume that we got
  -- called using `cabal exec`.
  -- Hopefully some day someone comes up with a saner way to test this.
  lookupEnv "GHC_ENVIRONMENT" <&> \case
    Just{} -> Process.proc exe args
    Nothing ->
      let
        withCompiler = case ghcVersion of
          Nothing -> []
          Just version -> ["--with-compiler", "ghc-" <> version]
       in
        Process.proc
          "cabal"
          (["exec", "-v0"] <> withCompiler <> ("--" : exe : args))

showCommandForUser :: ProcessConfig stdin stdout stderr -> String
showCommandForUser pc = case pcCmdSpec pc of
  Process.RawCommand exe args -> Process.showCommandForUser exe args
  Process.ShellCommand cli -> cli
