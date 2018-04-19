module Main ( main ) where

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.Function ( on )
import System.FilePath ( takeBaseName, replaceExtension )
import Test.Tasty ( defaultMain, TestTree, testGroup )
import Test.Tasty.Golden ( findByExtension )
import Test.Tasty.Golden.Advanced ( goldenTest )

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Verbosity as Cabal

import DhallToCabal ( dhallToCabal )


  
main :: IO ()
main =
  defaultMain =<< goldenTests



goldenTests :: IO TestTree
goldenTests = do
  dhallFiles <-
    findByExtension [ ".dhall" ] "golden-tests"

  return
    $ testGroup "dhall-to-cabal golden tests"
        [ goldenTest
            ( takeBaseName dhallFile )
            ( Cabal.readGenericPackageDescription Cabal.normal cabalFile )
            ( LazyText.readFile dhallFile >>= dhallToCabal dhallFile  )
            ( \expected actual ->
                return $
                  if on (==) Cabal.showGenericPackageDescription expected actual then
                    Nothing
                  else
                    Just "Generated .cabal file does not match input"
            )
            ( Cabal.writeGenericPackageDescription cabalFile )
        | dhallFile <- dhallFiles
        , let cabalFile = replaceExtension dhallFile ".cabal"
        ]



reverseArtifacts pkg =
  pkg { Cabal.executables = reverse (Cabal.executables pkg) }
