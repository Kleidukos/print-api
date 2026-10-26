module DuplicateExports (spec) where

import Control.Exception (finally)
import Data.List.Extra qualified as List
import System.Directory qualified as Directory
import System.Process.Typed qualified as Process
import Test.Tasty
import Test.Tasty.HUnit

import PrintApi.CLI.Cmd.Dump qualified as Dump
import Utils

spec :: TestTree
spec =
  testGroup
    "Duplicate exports"
    [ testCase
        "A pattern synonym exported twice is rendered once (issue #44)"
        testPatternSynonymRenderedOnce
    ]

-- | The patsyn-dup fixture exports the Bar pattern synonym both standalone
-- and bundled with its type, so it appears twice in modInfoExports. Before
-- the fix this rendered the pattern Bar declaration twice.
testPatternSynonymRenderedOnce :: Assertion
testPatternSynonymRenderedOnce = do
  originalDir <- Directory.getCurrentDirectory
  fixtureDir <- Directory.makeAbsolute "test/fixtures/patsyn-dup"
  libdir <- getLibdir
  flip finally (Directory.setCurrentDirectory originalDir) $ do
    Directory.setCurrentDirectory fixtureDir
    buildExit <-
      Process.runProcess $
        Process.shell "cabal build --write-ghc-environment-files=always"
    assertExitSuccess "Build patsyn-dup fixture" buildExit
    api <- Dump.computePackageAPI False libdir [] "patsyn-dup"
    let occurrences = length $ filter (List.isInfixOf "pattern Bar") (lines api)
    assertEqual "pattern Bar should be rendered exactly once" 1 occurrences
