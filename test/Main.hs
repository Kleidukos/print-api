module Main (main) where

import System.IO
import Test.Tasty
import Test.Tasty.Runners.Reporter qualified as Reporter

import IgnoreList qualified

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  defaultMainWithIngredients
    [Reporter.ingredient]
    $ testGroup
      "print-api tests"
      specs

specs :: [TestTree]
specs =
  [ IgnoreList.spec ]
