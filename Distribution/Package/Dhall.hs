{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

module Distribution.Package.Dhall where

import Data.Foldable ( toList )
import Data.Function ( (&) )
import Control.Monad ( (>=>) )

import qualified Distribution.Text as Cabal ( simpleParse )
import qualified Data.Map as Map
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Distribution.ModuleName as Cabal
import qualified Dhall
import qualified Dhall.Core as Dhall ( Expr )
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Distribution.License as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Types.Dependency as Cabal
import qualified Distribution.Types.ExecutableScope as Cabal
import qualified Distribution.Types.ForeignLib as Cabal
import qualified Distribution.Types.ForeignLibType as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Version as Cabal



packageIdentifier :: Dhall.Type Cabal.PackageIdentifier
packageIdentifier =
  let
    extract expr = do
      Expr.RecordLit fields <- return expr
      pkgName <- Map.lookup "name" fields >>= Dhall.extract packageName
      pkgVersion <- Map.lookup "version" fields >>= Dhall.extract version
      return Cabal.PackageIdentifier { .. }

    expected =
      Expr.Record
        ( Map.fromList
            [ ( "name", Expr.Text )
            , ( "version", Dhall.expected ( toList <$> Dhall.vector version ) )
            ] )

  in Dhall.Type { .. }



packageName :: Dhall.Type Cabal.PackageName
packageName =
  let
    extract =
      fmap Cabal.mkPackageName . exprToString

    expected =
      Expr.Text

  in Dhall.Type { .. }



packageDescription :: Dhall.Type Cabal.PackageDescription
packageDescription =
  let
    extract expr = do
      Expr.RecordLit fields <- return expr
      let field = flip Map.lookup fields
      package <- field "package" >>= Dhall.extract packageIdentifier
      benchmarks <-
        field "benchmarks"
          >>= fmap toList . Dhall.extract ( Dhall.vector benchmark )
      testSuites <-
        field "tests"
          >>= fmap toList . Dhall.extract ( Dhall.vector testSuite )
      executables <-
        field "executables"
          >>= fmap toList . Dhall.extract ( Dhall.vector executable )
      foreignLibs <- 
        field "foreign-libraries"
          >>= fmap toList . Dhall.extract ( Dhall.vector foreignLib )
      subLibraries <- return []
      library <-
        field "library"
          >>= Dhall.extract ( Dhall.maybe library )
      customFieldsPD <- do
        expr <- field "x-fields"
        pairs <- fmap toList ( Dhall.extract ( Dhall.vector Dhall.auto ) expr )
        return ( fmap ( \( a, b ) -> ( LazyText.unpack a, LazyText.unpack b ) ) pairs )
      sourceRepos <-
        field "source-repos"
          >>= fmap toList . Dhall.extract ( Dhall.vector sourceRepo )
      specVersionRaw <- return ( Left ( Cabal.mkVersion [ 2, 0 ] ) )
      buildType <- return ( Just Cabal.Simple )
      license <- return Cabal.OtherLicense
      licenseFiles <- return []
      copyright <- return ""
      maintainer <- return ""
      author <- return ""
      stability <- return ""
      testedWith <- return []
      homepage <- return ""
      pkgUrl <- return ""
      bugReports <- return ""
      synopsis <- return ""
      description <- return ""
      category <- return ""
      buildDepends <- return []
      setupBuildInfo <- return Nothing
      dataFiles <- return []
      dataDir <- return []
      extraSrcFiles <- return []
      extraTmpFiles <- return []
      extraDocFiles <- return []
      return Cabal.PackageDescription
        { .. }

    fieldTypes =
      [ ( "package", Dhall.expected packageIdentifier )
      , ( "benchmarks", Dhall.expected ( Dhall.vector benchmark ) )
      , ( "tests", Dhall.expected ( Dhall.vector testSuite ) )
      , ( "executables", Dhall.expected ( Dhall.vector executable ) )
      , ( "foreign-libraries", Dhall.expected ( Dhall.vector foreignLib ) )
      , ( "library", Dhall.expected ( Dhall.maybe library ) )
      , ( "x-fields", Dhall.expected ( Dhall.auto @[(LazyText.Text, LazyText.Text)] ) )
      , ( "source-repos", Dhall.expected ( Dhall.vector sourceRepo ) )
      ]

    expected =
      Expr.Record ( Map.fromList fieldTypes )

  in Dhall.Type { .. }



version :: Dhall.Type Cabal.Version
version =
  let
    naturalToInt expr = do
      Expr.NaturalLit n <- return expr
      return (fromIntegral n) 
    
    extract expr = do
      Expr.ListLit _ components <- return expr
      components
        & traverse naturalToInt
        & fmap (Cabal.mkVersion . toList)

    expected =
      Expr.Natural
        
  in Dhall.Type { .. }



benchmark :: Dhall.Type Cabal.Benchmark
benchmark =
  let
    extract expr = do
      Expr.RecordLit fields <- return expr
      mainIs <- Map.lookup "main-is" fields >>= Dhall.extract string
      benchmarkName <-
        Map.lookup "name" fields
          >>= Dhall.extract unqualComponentName
      benchmarkInterface <-
        return ( Cabal.BenchmarkExeV10 ( Cabal.mkVersion [ 1, 0 ] ) mainIs )
      benchmarkBuildInfo <- Dhall.extract buildInfo expr
      return Cabal.Benchmark { .. }

    expected =
      Expr.Record
        ( Map.fromList
            [ ( "name", Dhall.expected string )
            , ( "main-is", Dhall.expected string )
            ] )

  in Dhall.Type { .. }



string :: Dhall.Type String
string =
  LazyText.unpack <$> Dhall.lazyText



buildInfo :: Dhall.Type Cabal.BuildInfo
buildInfo =
  let
    extract expr = do
      Expr.RecordLit fields <- return expr
      buildable <- return True
      buildTools <- return []
      buildToolDepends <- return []
      cppOptions <- return []
      ccOptions <- return []
      ldOptions <- return []
      pkgconfigDepends <- return []
      frameworks <- return []
      extraFrameworkDirs <- return []
      cSources <- return []
      jsSources <- return []
      hsSourceDirs <- return []
      otherModules <-
        Map.lookup "other-modules" fields
          >>= fmap toList . Dhall.extract ( Dhall.vector moduleName )
      autogenModules <- return []
      defaultLanguage <- return Nothing
      otherLanguages <- return []
      defaultExtensions <- return []
      otherExtensions <- return []
      oldExtensions <- return []
      extraLibs <- return []
      extraGHCiLibs <- return []
      extraLibDirs <- return []
      includeDirs <- return []
      includes <- return []
      installIncludes <- return []
      options <- return []
      profOptions <- return []
      sharedOptions <- return []
      customFieldsBI <- return []
      targetBuildDepends <-
        Map.lookup "build-dependencies" fields
          >>= fmap toList . Dhall.extract ( Dhall.vector dependency )
      mixins <- return []
      return Cabal.BuildInfo { ..  }

    expected =
      Expr.Record buildInfoFields

  in Dhall.Type { .. }



buildInfoFields =
  Map.fromList
    [ ( "build-dependencies"
        , Dhall.expected ( Dhall.vector dependency ) )
    , ( "other-modules", Dhall.expected ( Dhall.vector moduleName ) )
    ]



testSuite :: Dhall.Type Cabal.TestSuite
testSuite =
  let
    extract expr = do
      Expr.RecordLit fields <- return expr
      testName <- Map.lookup "name" fields >>= Dhall.extract unqualComponentName
      mainIs <- Map.lookup "main-is" fields >>= Dhall.extract string
      testInterface <-
        return ( Cabal.TestSuiteExeV10 ( Cabal.mkVersion [ 1, 0 ] ) mainIs )
      testBuildInfo <- Dhall.extract buildInfo expr
      return Cabal.TestSuite { .. }

    expected =
      Expr.Record
        ( Map.union 
            ( Map.fromList
                [ ( "name", Dhall.expected string )
                , ( "main-is", Dhall.expected string )
                ] )
            buildInfoFields
        )

  in Dhall.Type { .. }


unqualComponentName :: Dhall.Type Cabal.UnqualComponentName
unqualComponentName =
  Cabal.mkUnqualComponentName <$> string



executable :: Dhall.Type Cabal.Executable
executable =
  let
    extract expr = do
      Expr.RecordLit fields <- return expr
      exeName <- Map.lookup "name" fields >>= Dhall.extract unqualComponentName
      modulePath <- Map.lookup "main-is" fields >>= Dhall.extract string
      exeScope <- return Cabal.ExecutablePublic
      buildInfo <- Dhall.extract buildInfo expr
      return Cabal.Executable { .. }

    expected =
      Expr.Record
        ( Map.union 
            ( Map.fromList
                [ ( "name", Dhall.expected string )
                , ( "main-is", Dhall.expected string )
                ] )
            buildInfoFields
        )

  in Dhall.Type { .. }



foreignLib :: Dhall.Type Cabal.ForeignLib
foreignLib =
  let
    extract expr = do
      Expr.RecordLit fields <- return expr
      foreignLibName <- Map.lookup "name" fields >>= Dhall.extract unqualComponentName
      foreignLibType <- Map.lookup "type" fields >>= Dhall.extract foreignLibType
      foreignLibOptions <- return []
      foreignLibBuildInfo <- Dhall.extract buildInfo expr
      foreignLibVersionInfo <- return Nothing
      foreignLibVersionLinux <- return Nothing
      foreignLibModDefFile <- return []
      return Cabal.ForeignLib { .. }

    expected =
      Expr.Record
        ( Map.fromList
            [ ( "name", Dhall.expected unqualComponentName )
            , ( "type", Dhall.expected foreignLibType )
            ] )

  in Dhall.Type { .. }



foreignLibType :: Dhall.Type Cabal.ForeignLibType
foreignLibType = 
  let
    extract expr = do
      Expr.UnionLit t _ _ <- return expr
      case t of
        "Shared" -> return Cabal.ForeignLibNativeShared
        _ -> Nothing

    expected =
      Expr.Union ( Map.fromList [ ( "Shared", Expr.Record Map.empty ) ] )

  in Dhall.Type { .. }



library :: Dhall.Type Cabal.Library
library =
  let
    extract expr = do
      Expr.RecordLit fields <- return expr
      libName <- Map.lookup "name" fields >>= Dhall.extract ( Dhall.maybe unqualComponentName )
      libBuildInfo <- Dhall.extract buildInfo expr
      exposedModules <- return []
      reexportedModules <- return []
      signatures <- return []
      libExposed <- return True
      return Cabal.Library { .. }

    expected =
      Expr.Record
        ( Map.union 
            ( Map.fromList
                [ ( "name", Dhall.expected ( Dhall.maybe unqualComponentName ) )
                ] )
            buildInfoFields
        )

  in Dhall.Type { .. }



sourceRepo :: Dhall.Type Cabal.SourceRepo
sourceRepo =
  let
    extract expr = do
      Expr.RecordLit fields <- return expr
      repoKind <- return Cabal.RepoHead
      repoType <- return Nothing
      repoLocation <- return Nothing
      repoModule <- return Nothing
      repoBranch <- return Nothing
      repoTag <- return Nothing
      repoSubdir <- return Nothing
      return Cabal.SourceRepo { .. }

    expected =
      Expr.Record Map.empty

  in Dhall.Type { .. }



dependency :: Dhall.Type Cabal.Dependency
dependency =
  let
    extract expr = do
      Expr.RecordLit fields <- return expr
      packageName <-
        Map.lookup "package" fields
          >>= Dhall.extract packageName
      versionRange <- return Cabal.anyVersion
      return ( Cabal.Dependency packageName versionRange )

    expected =
      Expr.Record ( Map.fromList [ ( "package", Dhall.expected packageName ) ] )

  in Dhall.Type { .. }



moduleName :: Dhall.Type Cabal.ModuleName
moduleName =
  let
    extract = do
      exprToString >=> Cabal.simpleParse

    expected =
      Expr.Text

  in Dhall.Type { .. }



exprToString :: Dhall.Expr a b -> Maybe String
exprToString expr = do
  Expr.TextLit builder <- return expr
  return
    ( LazyText.unpack ( Builder.toLazyText builder ) )
