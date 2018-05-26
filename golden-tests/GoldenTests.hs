module Main ( main ) where

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.Function ( on )
import System.FilePath ( takeBaseName, replaceExtension )
import Test.Tasty ( defaultMain, TestTree, testGroup )
import Test.Tasty.Golden ( findByExtension, goldenVsString )
import Test.Tasty.Golden.Advanced ( goldenTest )

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Dhall.Core
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Verbosity as Cabal

import CabalToDhall ( cabalToDhall )
import DhallToCabal ( dhallToCabal )


  
main :: IO ()
main =
  defaultMain =<< goldenTests



goldenTests :: IO TestTree
goldenTests = do
  dhallFiles <-
    findByExtension [ ".dhall" ] "golden-tests/dhall-to-cabal"
  cabalFiles <-
    findByExtension [ ".cabal" ] "golden-tests/cabal-to-dhall"

  return
    $ testGroup "golden tests"
      [ testGroup "dhall-to-cabal"
          [ goldenTest
              ( takeBaseName dhallFile )
              ( Cabal.readGenericPackageDescription Cabal.normal cabalFile )
              ( LazyText.readFile dhallFile >>= dhallToCabal dhallFile  )
              ( \expected actual -> do
                  let [exp,act] = map Cabal.showGenericPackageDescription
                                  [expected, actual]
                  if exp == act then
                      return Nothing
                  else do
                    putStrLn $ "Diff between expected " ++ cabalFile ++
                               " and actual " ++ dhallFile ++ " :"
                    let gDiff = getGroupedDiff (lines exp) (lines act)
                    putStrLn $ ppDiff gDiff
                    return $ Just "Generated .cabal file does not match input"
              )
              ( Cabal.writeGenericPackageDescription cabalFile )
          | dhallFile <- dhallFiles
          , let cabalFile = replaceExtension dhallFile ".cabal"
          ]
     , testGroup "cabal-to-dhall"
         [ goldenVsString
             ( takeBaseName cabalFile )
             dhallFile
             ( LazyText.readFile cabalFile
                 >>= fmap ( LazyText.encodeUtf8 . Dhall.Core.pretty ) . cabalToDhall
             )
         | cabalFile <- cabalFiles
         , let dhallFile = replaceExtension cabalFile ".dhall"
         ]
    ]

reverseArtifacts pkg =
  pkg { Cabal.executables = reverse (Cabal.executables pkg) }
