{-# LANGUAGE QuasiQuotes #-}
module IgnoreList where

import Control.Monad.IO.Class
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.List.Extra qualified as List
import Language.Haskell.Syntax.Module.Name (mkModuleName)
import System.Process.Typed qualified as Process
import Test.Tasty
import System.OsPath
import Test.Tasty.Golden
import Utils
import qualified Data.ByteString.Lazy as ByteString
import qualified System.Directory as Directory 

import qualified PrintApi.CLI.Cmd.Dump as Dump
import qualified System.Directory.OsPath as OsPath
import qualified System.IO as System
import qualified System.OsPath as OsPath

diffCmd :: String -> String -> [String]
diffCmd ref new = ["diff", "-u", ref, new]

spec :: TestTree
spec = testGroup "Ignore list"
  [ goldenVsStringDiff
        "User-supplied ignore list"
        diffCmd
        "test/golden/servant-client-expected-api.txt"
        generateServantClientAPIWithIgnoreList
  ]

generateServantClientAPIWithIgnoreList :: (MonadIO m) => m LazyByteString
generateServantClientAPIWithIgnoreList = do
  (exitCode, stdOut, _stdErr) <- Process.readProcess $ Process.shell "cabal exec -v0 -- ghc --print-libdir"
  assertExitSuccess "`cabal exec -v0 -- ghc --print-libdir`" exitCode
  assertExitSuccess "Fetch the archive of servant-client" =<< Process.runProcess (Process.shell "cabal get servant-client-0.20 --destdir=../")
  liftIO $ Directory.setCurrentDirectory "../servant-client-0.20"
  let buildServantClient = Process.shell "cabal build --write-ghc-environment-files=always"
  assertExitSuccess "Build servant-client" =<< Process.runProcess buildServantClient
  ignoreListPath <- liftIO $ OsPath.makeAbsolute [osp|../print-api/test/golden/servant-client-ignore-list.txt|]
  ignoreListFilePath <- liftIO $ OsPath.decodeUtf ignoreListPath
  modules <- lines <$> liftIO (System.readFile ignoreListFilePath)
  let ignoredModules = List.map mkModuleName modules
  actualAPI <- liftIO $ Dump.computePackageAPI (List.trimEnd $ C8.unpack stdOut) ignoredModules "servant-client"
  actualApiPath <- liftIO $ Directory.makeAbsolute "../print-api/test/golden/servant-client-actual-api.txt"
  liftIO $ System.writeFile actualApiPath actualAPI
  liftIO $ ByteString.readFile actualApiPath
