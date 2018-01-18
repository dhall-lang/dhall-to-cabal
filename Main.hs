module Main (main) where

import Data.Function ( (&) )
import System.Environment ( getArgs )

import Distribution.Package.Dhall

import qualified Data.Text.Lazy.IO as LazyText
import qualified Dhall
import qualified Distribution.PackageDescription.PrettyPrint as Cabal

main :: IO ()
main = do
  filePath : _ <- getArgs
  LazyText.readFile filePath
    >>= Dhall.detailed . Dhall.input packageDescription
    & fmap Cabal.showPackageDescription
    >>= putStrLn
