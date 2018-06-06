{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main ( main ) where

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.Function ( (&) )
import Lens.Micro ( set )
import System.FilePath ( takeBaseName, takeDirectory, replaceExtension )
import Test.Tasty ( defaultMain, TestTree, testGroup )
import Test.Tasty.Golden ( writeBinaryFile, findByExtension, goldenVsStringDiff )
import Test.Tasty.Golden.Advanced ( goldenTest )

import qualified Data.ByteString as BS
import qualified Data.Text.IO as StrictText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Dhall
import qualified Dhall.Core
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import qualified Distribution.Verbosity as Cabal

import CabalToDhall ( cabalToDhall, parseGenericPackageDescriptionThrows )
import DhallLocation ( DhallLocation ( DhallLocation ) )
import DhallToCabal ( dhallToCabal )


  
main :: IO ()
main =
  defaultMain =<< goldenTests


preludeLocation :: Dhall.Core.Import
preludeLocation =
  Dhall.Core.Import
    { Dhall.Core.importHashed =
        Dhall.Core.ImportHashed
          { Dhall.Core.hash =
              Nothing
          , Dhall.Core.importType =
              Dhall.Core.Local
                Dhall.Core.Here
                ( Dhall.Core.File
                   ( Dhall.Core.Directory [ "dhall", "..", ".." ] )
                   "prelude.dhall"
                )
          }
    , Dhall.Core.importMode =
        Dhall.Core.Code
    }


typesLocation :: Dhall.Core.Import
typesLocation =
  Dhall.Core.Import
    { Dhall.Core.importHashed =
        Dhall.Core.ImportHashed
          { Dhall.Core.hash =
              Nothing
          , Dhall.Core.importType =
              Dhall.Core.Local
                Dhall.Core.Here
                ( Dhall.Core.File
                   ( Dhall.Core.Directory [ "dhall", "..", ".." ] )
                   "types.dhall"
                )
          }
    , Dhall.Core.importMode =
        Dhall.Core.Code
    }


goldenTests :: IO TestTree
goldenTests = do
  -- Note: must remain in sync with the layout options in
  -- cabal-to-dhall/Main.hs, so that test output is easy to generate
  -- at the command line.
  let layoutOpts = Pretty.defaultLayoutOptions
        { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }
      dhallLocation = DhallLocation preludeLocation typesLocation

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
              ( \ ( Cabal.showGenericPackageDescription -> exp )
                  ( Cabal.showGenericPackageDescription -> act ) -> do
                  if exp == act then
                    return Nothing
                  else do
                    let gDiff =  getGroupedDiff (lines exp) (lines act)
                        ppDiff' = ppDiff gDiff
                        output = testFailedDiffOutput cabalFile dhallFile ppDiff'
                    return $ Just output
              )
              ( Cabal.writeGenericPackageDescription cabalFile )
          | dhallFile <- dhallFiles
          , let cabalFile = replaceExtension dhallFile ".cabal"
                settings = Dhall.defaultInputSettings
                  & set Dhall.rootDirectory ( takeDirectory dhallFile )
                  & set Dhall.sourceName dhallFile
          ]
     , testGroup "cabal-to-dhall"
         [ goldenTest
             ( takeBaseName cabalFile )
             ( LazyText.readFile dhallFile )
             ( BS.readFile cabalFile >>= parseGenericPackageDescriptionThrows
                 & fmap ( Pretty.renderLazy
                        . Pretty.layoutSmart layoutOpts . Pretty.pretty
                        . cabalToDhall dhallLocation
                        )
             )
             ( \ (LazyText.unpack -> exp) (LazyText.unpack -> act) -> do
                let gDiff   = getGroupedDiff (lines exp) (lines act)
                    ppDiff' = ppDiff gDiff
                    output  = testFailedDiffOutput dhallFile cabalFile ppDiff'
                return $ if  ppDiff' == "\n" then Nothing
                         else Just output
              )
              ( LazyText.writeFile dhallFile )
         | cabalFile <- cabalFiles
         , let dhallFile = replaceExtension cabalFile ".dhall"
         ]
    ]

testFailedDiffOutput :: FilePath -> FilePath -> String -> String
testFailedDiffOutput expFile actFile ppDiff =
  unlines [ "Test output was different from '" ++ expFile ++ "'."                            , "Output of diff between '" ++ expFile ++ "' " ++
            "and test output using '" ++ actFile ++ "':"
          , ppDiff
          ]  
