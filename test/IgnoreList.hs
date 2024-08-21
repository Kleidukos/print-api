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

import qualified System.Directory.OsPath as OsPath
import qualified System.IO as System
import qualified System.OsPath as OsPath
import Control.Monad.Extra (whenM)

import qualified PrintApi.CLI.Cmd.Dump as Dump

diffCmd :: String -> String -> [String]
diffCmd ref new = ["diff", "-u", ref, new]

spec :: TestTree
spec = testGroup "Ignore list"
  [ goldenVsStringDiff
        "User-supplied ignore list"
        diffCmd
        "test/golden/vector-expected-api.txt"
        generateVectorAPIWithIgnoreList
  ]

generateVectorAPIWithIgnoreList :: (MonadIO m) => m LazyByteString
generateVectorAPIWithIgnoreList = do
  let vectorPath = "../vector-0.13.1.0"
  liftIO $ whenM (Directory.doesDirectoryExist vectorPath) $ 
    Directory.removeDirectoryRecursive vectorPath
  (exitCode, stdOut, _stdErr) <- Process.readProcess $ Process.shell "cabal exec -v0 -- ghc --print-libdir"
  assertExitSuccess "`cabal exec -v0 -- ghc --print-libdir`" exitCode
  assertExitSuccess "Fetch the archive of vector" =<< Process.runProcess (Process.shell "cabal get vector-0.13.1.0 --destdir=../")
  liftIO $ Directory.setCurrentDirectory vectorPath
  let buildVector = Process.shell "cabal build -j --write-ghc-environment-files=always"
  assertExitSuccess "Build vector" =<< Process.runProcess buildVector
  ignoreListPath <- liftIO $ OsPath.makeAbsolute [osp|../print-api/test/golden/vector-ignore-list.txt|]
  ignoreListFilePath <- liftIO $ OsPath.decodeUtf ignoreListPath
  modules <- lines <$> liftIO (System.readFile ignoreListFilePath)
  let ignoredModules = List.map mkModuleName modules
  actualAPI <- liftIO $ Dump.computePackageAPI (List.trimEnd $ C8.unpack stdOut) ignoredModules "vector"
  actualApiPath <- liftIO $ Directory.makeAbsolute "../print-api/test/golden/vector-actual-api.txt"
  liftIO $ System.writeFile actualApiPath actualAPI
  liftIO $ ByteString.readFile actualApiPath
