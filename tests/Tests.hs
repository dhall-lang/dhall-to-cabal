module Main where

import qualified DhallToCabal.Tests
import qualified Test.Tasty


main :: IO ()
main =
  Test.Tasty.defaultMain DhallToCabal.Tests.versionSpec
