module Main (main) where

import System.IO
import Test.Tasty
import Test.Tasty.Runners (NumThreads (..))
import Test.Tasty.Runners.Reporter qualified as Reporter

import DuplicateExports qualified
import IgnoreList qualified

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  defaultMainWithIngredients
    [Reporter.ingredient]
    -- These tests mutate the process-global working directory, so they must
    -- not run concurrently.
    $ localOption (NumThreads 1)
    $ testGroup
      "print-api tests"
      specs

specs :: [TestTree]
specs =
  [ IgnoreList.spec
  , DuplicateExports.spec
  ]
