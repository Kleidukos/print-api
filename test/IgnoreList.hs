{-# LANGUAGE QuasiQuotes #-}
module IgnoreList where

import Data.ByteString.Lazy (LazyByteString)
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
import Control.Exception (finally)
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

generateVectorAPIWithIgnoreList :: IO LazyByteString
generateVectorAPIWithIgnoreList = do
  originalDir <- Directory.getCurrentDirectory
  flip finally (Directory.setCurrentDirectory originalDir) $ do
    let vectorPath = "../vector-0.13.1.0"
    whenM (Directory.doesDirectoryExist vectorPath) $
      Directory.removeDirectoryRecursive vectorPath
    libdir <- getLibdir
    assertExitSuccess "Fetch the archive of vector" =<< Process.runProcess (Process.shell "cabal get vector-0.13.1.0 --destdir=../")
    Directory.setCurrentDirectory vectorPath
    let buildVector = Process.shell "cabal build -j --write-ghc-environment-files=always --ghc-options=-haddock"
    assertExitSuccess "Build vector" =<< Process.runProcess buildVector
    ignoreListPath <- OsPath.makeAbsolute [osp|../print-api/test/golden/vector-ignore-list.txt|]
    ignoreListFilePath <- OsPath.decodeUtf ignoreListPath
    modules <- lines <$> System.readFile ignoreListFilePath
    let ignoredModules = List.map mkModuleName modules
    actualAPI <- Dump.computePackageAPI False libdir ignoredModules "vector"
    actualApiPath <- Directory.makeAbsolute "../print-api/test/golden/vector-actual-api.txt"
    System.writeFile actualApiPath actualAPI
    ByteString.readFile actualApiPath
