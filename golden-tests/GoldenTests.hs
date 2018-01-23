-- Taken from https://ro-che.info/articles/2017-12-04-golden-tests

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import Distribution.Package.Dhall (dhallFileToCabal)
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Verbosity as Cabal
import System.FilePath (takeBaseName, replaceExtension)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (findByExtension)
import Test.Tasty.Golden.Advanced (goldenTest)
  
main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  dhallFiles <- findByExtension [".dhall"] "golden-tests"
  return $ testGroup "dhall-to-cabal golden tests"
    [ goldenTest
        (takeBaseName dhallFile) -- test name
        (reverseArtifacts . Cabal.flattenPackageDescription <$> Cabal.readGenericPackageDescription Cabal.normal cabalFile)
        (dhallFileToCabal dhallFile)
        (\expected actual -> return $
            if Cabal.showPackageDescription expected == Cabal.showPackageDescription actual then
              Nothing
            else
              Just $
                "Generated .cabal file does not match input:\n\n" ++
                "Differences:\n" ++
                ppDiff
                  (getGroupedDiff (lines (Cabal.showPackageDescription actual))
                                  (lines (Cabal.showPackageDescription expected)))
        )
        (Cabal.writePackageDescription cabalFile)
    | dhallFile <- dhallFiles
    , let cabalFile = replaceExtension dhallFile ".cabal"
    ]

reverseArtifacts pkg =
  pkg { Cabal.executables = reverse (Cabal.executables pkg) }
