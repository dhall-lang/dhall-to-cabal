-- Taken from https://ro-che.info/articles/2017-12-04-golden-tests

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import Distribution.Package.Dhall (dhallFileToCabal)
import qualified Distribution.PackageDescription.Configuration as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
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
        (Cabal.flattenPackageDescription <$> Cabal.readGenericPackageDescription Cabal.normal cabalFile)
        (dhallFileToCabal dhallFile)
        (\expected actual -> return $
            if Cabal.showPackageDescription expected == Cabal.showPackageDescription actual then
              Nothing
            else
              Just $
                "Generated .cabal file does not match input:\n\n" ++
                "Got:\n\n" ++
                Cabal.showPackageDescription actual ++
                "\n\nExpected:\n\n" ++
                Cabal.showPackageDescription expected)
        (Cabal.writePackageDescription cabalFile)
    | dhallFile <- dhallFiles
    , let cabalFile = replaceExtension dhallFile ".cabal"
    ]
