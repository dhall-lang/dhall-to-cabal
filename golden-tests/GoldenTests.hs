-- Taken from https://ro-che.info/articles/2017-12-04-golden-tests

import qualified Data.ByteString.Lazy as LBS
import Distribution.Package.Dhall (dhallFileToCabal)
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import System.FilePath (takeBaseName, replaceExtension)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
  
main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  dhallFiles <- findByExtension [".dhall"] "golden-tests"
  return $ testGroup "dhall-to-cabal golden tests"
    [ goldenVsString
        (takeBaseName dhallFile) -- test name
        cabalFile -- golden file path
        (LazyText.encodeUtf8 . LazyText.pack . Cabal.showPackageDescription <$> dhallFileToCabal dhallFile) -- action whose result is tested
    | dhallFile <- dhallFiles
    , let cabalFile = replaceExtension dhallFile ".cabal"
    ]
