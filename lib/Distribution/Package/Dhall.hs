{-# language ApplicativeDo #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module Distribution.Package.Dhall ( dhallFileToCabal ) where

import Control.Exception ( Exception, throwIO )
import Data.Function ( (&) )
import Data.Monoid ( (<>) )
import Data.Text.Buildable ( Buildable(..) )
import Text.Trifecta.Delta ( Delta(..) )

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Dhall
import qualified Dhall.Context as Ctx
import qualified Dhall.Core
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Distribution.Compiler as Cabal
import qualified Distribution.License as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.System as Cabal ( Arch(..), OS(..) )
import qualified Distribution.Text as Cabal ( simpleParse )
import qualified Distribution.Types.CondTree as Cabal
import qualified Distribution.Types.Dependency as Cabal
import qualified Distribution.Types.ExeDependency as Cabal
import qualified Distribution.Types.ExecutableScope as Cabal
import qualified Distribution.Types.ForeignLib as Cabal
import qualified Distribution.Types.ForeignLibOption as Cabal
import qualified Distribution.Types.ForeignLibType as Cabal
import qualified Distribution.Types.LegacyExeDependency as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PkgconfigDependency as Cabal
import qualified Distribution.Types.PkgconfigName as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Version as Cabal
import qualified Language.Haskell.Extension as Cabal

import qualified Dhall.Core as Expr
  ( Const(..), Expr(..), Var(..) )

import Dhall.Extra



packageIdentifier :: Dhall.Type Cabal.PackageIdentifier
packageIdentifier =
  makeRecord $ do
    pkgName <-
      keyValue "name" packageName

    pkgVersion <-
      keyValue "version" version

    pure Cabal.PackageIdentifier { .. }



packageName :: Dhall.Type Cabal.PackageName
packageName =
  Cabal.mkPackageName <$> Dhall.string



packageDescription :: RecordBuilder Cabal.PackageDescription
packageDescription = do
  package <-
    keyValue "package" packageIdentifier

  benchmarks <-
    pure []

  testSuites <-
    pure []

  executables <-
    pure []

  foreignLibs <-
    pure []

  subLibraries <-
    pure []

  library <-
    pure Nothing

  customFieldsPD <-
    keyValue
      "x-fields"
      ( Dhall.list ( Dhall.pair Dhall.string Dhall.string ) )

  sourceRepos <-
    keyValue "source-repos" ( Dhall.list sourceRepo )

  specVersionRaw <-
    Left <$> ( keyValue "cabal-version" version )

  buildType <-
    keyValue "build-type" ( Dhall.maybe buildType )

  license <-
    keyValue "license" license

  licenseFiles <-
    keyValue "license-files" ( Dhall.list Dhall.string )

  copyright <-
    keyValue "copyright" Dhall.string

  maintainer <-
    keyValue "maintainer" Dhall.string

  author <-
    keyValue "author" Dhall.string

  stability <-
    keyValue "stability" Dhall.string

  testedWith <-
    keyValue "tested-with" ( Dhall.list compiler )

  homepage <-
    keyValue "homepage" Dhall.string

  pkgUrl <-
    keyValue "package-url" Dhall.string

  bugReports <-
    keyValue "bug-reports" Dhall.string

  synopsis <-
    keyValue "synopsis" Dhall.string

  description <-
    keyValue "description" Dhall.string

  category <-
    keyValue "category" Dhall.string

  -- Cabal documentation states
  --
  --   > YOU PROBABLY DON'T WANT TO USE THIS FIELD.
  --
  -- So I guess we won't use this field.
  buildDepends <-
    pure []

  setupBuildInfo <-
    pure Nothing

  dataFiles <-
    keyValue "data-files" ( Dhall.list Dhall.string )

  dataDir <-
    keyValue "data-directory" Dhall.string

  extraSrcFiles <-
    keyValue "extra-source-files" ( Dhall.list Dhall.string )

  extraTmpFiles <-
    keyValue "extra-temp-files" ( Dhall.list Dhall.string )

  extraDocFiles <-
    keyValue "extra-doc-files" ( Dhall.list Dhall.string )

  return Cabal.PackageDescription { .. }



version :: Dhall.Type Cabal.Version
version =
  Cabal.mkVersion <$> Dhall.list ( fromIntegral <$> Dhall.natural )



benchmark :: Dhall.Type Cabal.Benchmark
benchmark =
  makeRecord $ do
    mainIs <-
      keyValue "main-is" Dhall.string

    benchmarkName <-
      pure ""

    benchmarkBuildInfo <-
      buildInfo

    pure
      Cabal.Benchmark
        { benchmarkInterface =
            Cabal.BenchmarkExeV10 ( Cabal.mkVersion [ 1, 0 ] ) mainIs
        , ..
        }



buildInfo :: RecordBuilder Cabal.BuildInfo
buildInfo = do
  buildable <-
    keyValue "buildable" Dhall.bool

  buildTools <-
    keyValue "build-tools" ( Dhall.list legacyExeDependency )

  buildToolDepends <-
    keyValue "build-tool-depends" ( Dhall.list exeDependency )

  cppOptions <-
    keyValue "cpp-options" ( Dhall.list Dhall.string )

  ccOptions <-
    keyValue "cc-options" ( Dhall.list Dhall.string )

  ldOptions <-
    keyValue "ld-options" ( Dhall.list Dhall.string )

  pkgconfigDepends <-
    keyValue "pkgconfig-depends" ( Dhall.list pkgconfigDependency )

  frameworks <-
    keyValue "frameworks" ( Dhall.list Dhall.string )

  extraFrameworkDirs <-
    keyValue "extra-framework-dirs" ( Dhall.list Dhall.string )

  cSources <-
    keyValue "c-sources" ( Dhall.list Dhall.string )

  jsSources <-
    keyValue "js-sources" ( Dhall.list Dhall.string )

  hsSourceDirs <-
    keyValue "hs-source-dirs" ( Dhall.list Dhall.string )

  otherModules <-
    keyValue "other-modules" ( Dhall.list moduleName )

  autogenModules <-
    keyValue "autogen-modules" ( Dhall.list moduleName )

  defaultLanguage <-
    keyValue "default-language" ( Dhall.maybe language )

  otherLanguages <-
    keyValue "other-languages" ( Dhall.list language )

  defaultExtensions <-
    keyValue "default-extensions" ( Dhall.list extension )

  otherExtensions <-
    keyValue "other-extensions" ( Dhall.list extension )

  oldExtensions <-
    pure []

  extraLibs <-
    keyValue "extra-libraries" ( Dhall.list Dhall.string )

  extraGHCiLibs <-
    keyValue "extra-ghci-libraries" ( Dhall.list Dhall.string )

  extraLibDirs <-
    keyValue "extra-lib-dirs" ( Dhall.list Dhall.string )

  includeDirs <-
    keyValue "include-dirs" ( Dhall.list Dhall.string )

  includes <-
    keyValue "include" ( Dhall.list Dhall.string )

  installIncludes <-
    keyValue "install-includes" ( Dhall.list Dhall.string )

  options <-
    keyValue "compiler-options" compilerOptions

  profOptions <-
    keyValue "profiling-options" compilerOptions

  sharedOptions <-
    keyValue "shared-options" compilerOptions

  customFieldsBI <-
    pure []

  targetBuildDepends <-
    keyValue "build-dependencies" ( Dhall.list dependency )

  mixins <-
    pure []

  return Cabal.BuildInfo { ..  }



testSuite :: Dhall.Type Cabal.TestSuite
testSuite =
  makeRecord $ do
    testName <-
      pure ""

    mainIs <-
      keyValue "main-is" Dhall.string

    testBuildInfo <-
      buildInfo

    pure
      Cabal.TestSuite
        { testInterface =
            Cabal.TestSuiteExeV10 ( Cabal.mkVersion [ 1, 0 ] ) mainIs
        , ..
        }



unqualComponentName :: Dhall.Type Cabal.UnqualComponentName
unqualComponentName =
  Cabal.mkUnqualComponentName <$> Dhall.string



executable :: Dhall.Type Cabal.Executable
executable =
  makeRecord $ do
    exeName <-
      pure ""

    modulePath <-
      keyValue "main-is" Dhall.string

    exeScope <-
      keyValue "scope" executableScope

    buildInfo <-
      buildInfo

    pure Cabal.Executable { .. }



foreignLib :: Dhall.Type Cabal.ForeignLib
foreignLib =
  makeRecord $ do
    foreignLibName <-
      pure ""

    foreignLibType <-
      keyValue "type" foreignLibType

    foreignLibOptions <-
      keyValue "options" ( Dhall.list foreignLibOption )

    foreignLibBuildInfo <-
      buildInfo

    foreignLibVersionInfo <-
      keyValue "version-info" ( Dhall.maybe versionInfo )

    foreignLibVersionLinux <-
      keyValue "linux-version" ( Dhall.maybe version )

    foreignLibModDefFile <-
      keyValue "module-definition-files" ( Dhall.list Dhall.string )

    pure Cabal.ForeignLib { .. }



foreignLibType :: Dhall.Type Cabal.ForeignLibType
foreignLibType =
  makeUnion
    ( Map.fromList
        [ ( "Shared", Cabal.ForeignLibNativeShared <$ Dhall.unit )
        , ( "Static", Cabal.ForeignLibNativeStatic <$ Dhall.unit )
        ]
    )



library :: Dhall.Type Cabal.Library
library =
  makeRecord $ do
    libName <-
      pure Nothing

    libBuildInfo <-
      buildInfo

    exposedModules <-
      keyValue "exposed-modules" ( Dhall.list moduleName )

    reexportedModules <-
      keyValue "reexported-modules" ( Dhall.list moduleReexport )

    signatures <-
      pure []

    libExposed <-
      pure True

    pure Cabal.Library { .. }



sourceRepo :: Dhall.Type Cabal.SourceRepo
sourceRepo =
  makeRecord $ do
    repoKind <-
      pure Cabal.RepoHead

    repoType <-
      keyValue "type" ( Dhall.maybe repoType )

    repoLocation <-
      keyValue "location" ( Dhall.maybe Dhall.string )

    repoModule <-
      pure Nothing

    repoBranch <-
      pure Nothing

    repoTag <-
      pure Nothing

    repoSubdir <-
      pure Nothing

    pure Cabal.SourceRepo { .. }



dependency :: Dhall.Type Cabal.Dependency
dependency =
  makeRecord $ do
    packageName <-
      keyValue "package" packageName

    versionRange <-
      keyValue "bounds" versionRange

    pure ( Cabal.Dependency packageName versionRange )



moduleName :: Dhall.Type Cabal.ModuleName
moduleName =
  validateType $
    Cabal.simpleParse <$> Dhall.string



dhallFileToCabal :: FilePath -> IO Cabal.GenericPackageDescription
dhallFileToCabal file = do
  source <-
    LazyText.readFile file

  Dhall.detailed ( input source genericPackageDescription )



input :: LazyText.Text -> Dhall.Type a -> IO a
input source t = do
  delta <-
    return ( Directed "(input)" 0 0 0 0 )

  expr  <-
    throws ( Dhall.Parser.exprFromText delta source )

  expr' <-
    Dhall.Import.loadWithContext cabalContext expr

  let
    suffix =
      Dhall.expected t
        & build
        & Builder.toLazyText
        & LazyText.encodeUtf8
        & LazyByteString.toStrict

  let
    annot =
      case expr' of
        Expr.Note ( Dhall.Parser.Src begin end bytes ) _ ->
          Expr.Note
            ( Dhall.Parser.Src begin end bytes' )
            ( Expr.Annot expr' ( Dhall.expected t ) )

          where

          bytes' =
            bytes <> " : " <> suffix

        _ ->
          Expr.Annot expr' ( Dhall.expected t )

  _ <-
    throws (Dhall.TypeCheck.typeWith cabalContext annot)

  case Dhall.extract t ( Dhall.Core.normalize expr' ) of
    Just x  ->
      return x

    Nothing ->
      throwIO Dhall.InvalidType

  where

    throws :: Exception e => Either e a -> IO a
    throws =
      either throwIO return



cabalContext
  :: Ctx.Context ( Expr.Expr Dhall.Parser.Src Dhall.TypeCheck.X )
cabalContext =
  Ctx.empty
    & Ctx.insert "anyVersion" ( Dhall.expected versionRange )
    & Ctx.insert "noVersion" ( Dhall.expected versionRange )
    & Ctx.insert
        "thisVersion"
        ( Expr.Pi "_"
            ( Dhall.expected version )
            ( Dhall.expected versionRange )
        )
    & Ctx.insert
        "notThisVersion"
        ( Expr.Pi
            "_"
            ( Dhall.expected version )
            ( Dhall.expected versionRange )
        )
    & Ctx.insert
        "laterVersion"
        ( Expr.Pi
            "_"
            ( Dhall.expected version )
            ( Dhall.expected versionRange )
        )
    & Ctx.insert
        "earlierVersion"
        ( Expr.Pi
            "_"
            ( Dhall.expected version )
            ( Dhall.expected versionRange )
        )
    & Ctx.insert
        "orLaterVersion"
        ( Expr.Pi
            "_"
            ( Dhall.expected version )
            ( Dhall.expected versionRange )
        )
    & Ctx.insert
        "orEarlierVersion"
        ( Expr.Pi
            "_"
            ( Dhall.expected version )
            ( Dhall.expected versionRange )
        )
    & Ctx.insert
        "unionVersionRanges"
        ( Expr.Pi
            "_"
            ( Dhall.expected versionRange )
            ( Expr.Pi
                "_"
                ( Dhall.expected versionRange )
                ( Dhall.expected versionRange  )
            )
        )
    & Ctx.insert
        "intersectVersionRanges"
        ( Expr.Pi
            "_"
            ( Dhall.expected versionRange )
            ( Expr.Pi
                "_"
                ( Dhall.expected versionRange )
                ( Dhall.expected versionRange  )
            )
        )
    & Ctx.insert
        "differenceVersionRanges"
        ( Expr.Pi
            "_"
            ( Dhall.expected versionRange )
            ( Expr.Pi
                "_"
                ( Dhall.expected versionRange )
                ( Dhall.expected versionRange  )
            )
        )
    & Ctx.insert
        "invertVersionRange"
        ( Expr.Pi
            "_"
            ( Dhall.expected versionRange )
            ( Expr.Pi
                "_"
                ( Dhall.expected versionRange )
                ( Dhall.expected versionRange  )
            )
        )
    & Ctx.insert
        "withinVersion"
        ( Expr.Pi
            "_"
            ( Dhall.expected version )
            ( Dhall.expected versionRange )
        )
    & Ctx.insert
        "majorBoundVersion"
        ( Expr.Pi
            "_"
            ( Dhall.expected version )
            ( Dhall.expected versionRange )
        )
    & Ctx.insert "VersionRange" ( Expr.Const Expr.Type )



versionRange :: Dhall.Type Cabal.VersionRange
versionRange =
  let
    extract =
      \case
        Expr.Var ( Expr.V "anyVersion" 0 ) ->
          return Cabal.anyVersion

        Expr.Var ( Expr.V "noVersion" 0 ) ->
          return Cabal.noVersion

        Expr.App ( Expr.Var ( Expr.V "thisVersion" 0 ) ) components ->
          Cabal.thisVersion <$> Dhall.extract version components

        Expr.App ( Expr.Var ( Expr.V "notThisVersion" 0 ) ) components ->
          Cabal.notThisVersion <$> Dhall.extract version components

        Expr.App ( Expr.Var ( Expr.V "laterVersion" 0 ) ) components ->
          Cabal.laterVersion <$> Dhall.extract version components

        Expr.App ( Expr.Var ( Expr.V "earlierVersion" 0 ) ) components ->
          Cabal.earlierVersion <$> Dhall.extract version components

        Expr.App ( Expr.Var ( Expr.V "orLaterVersion" 0 ) ) components ->
          Cabal.orLaterVersion <$> Dhall.extract version components

        Expr.App ( Expr.Var ( Expr.V "orEarlierVersion" 0 ) ) components ->
          Cabal.orEarlierVersion <$> Dhall.extract version components

        Expr.App ( Expr.App ( Expr.Var ( Expr.V "unionVersionRanges" 0 ) ) a ) b ->
          Cabal.unionVersionRanges
            <$> Dhall.extract versionRange a
            <*> Dhall.extract versionRange b

        Expr.App ( Expr.App ( Expr.Var ( Expr.V "intersectVersionRanges" 0 ) ) a ) b ->
          Cabal.intersectVersionRanges
            <$> Dhall.extract versionRange a
            <*> Dhall.extract versionRange b

        Expr.App ( Expr.App ( Expr.Var ( Expr.V "differenceVersionRanges" 0 ) ) a ) b ->
          Cabal.differenceVersionRanges
            <$> Dhall.extract versionRange a
            <*> Dhall.extract versionRange b

        Expr.App ( Expr.Var ( Expr.V "invertVersionRange" 0 ) ) components ->
          Cabal.invertVersionRange <$> Dhall.extract versionRange components

        Expr.App ( Expr.Var ( Expr.V "withinVersion" 0 ) ) components ->
          Cabal.withinVersion <$> Dhall.extract version components

        Expr.App ( Expr.Var ( Expr.V "majorBoundVersion" 0 ) ) components ->
          Cabal.majorBoundVersion <$> Dhall.extract version components

        _ ->
          Nothing

    expected =
      Expr.Var ( Expr.V "VersionRange" 0 )

  in Dhall.Type { .. }



buildType :: Dhall.Type Cabal.BuildType
buildType =
  makeUnion
    ( Map.fromList
        [ ( "Simple", Cabal.Simple <$ Dhall.unit )
        , ( "Configure", Cabal.Configure <$ Dhall.unit )
        , ( "Make", Cabal.Make <$ Dhall.unit )
        , ( "Custom", Cabal.Custom <$ Dhall.unit )
        ]
    )



license :: Dhall.Type Cabal.License
license =
  makeUnion
    ( Map.fromList
        [ ( "GPL", Cabal.GPL <$> Dhall.maybe version )
        , ( "AGPL", Cabal.AGPL <$> Dhall.maybe version )
        , ( "LGPL", Cabal.LGPL <$> Dhall.maybe version )
        , ( "BSD2", Cabal.BSD2 <$ Dhall.unit )
        , ( "BSD3", Cabal.BSD3 <$ Dhall.unit )
        , ( "BSD4", Cabal.BSD4 <$ Dhall.unit )
        , ( "MIT", Cabal.MIT <$ Dhall.unit )
        , ( "ISC", Cabal.ISC <$ Dhall.unit )
        , ( "MPL", Cabal.MPL <$> version )
        , ( "Apache", Cabal.Apache <$> Dhall.maybe version )
        , ( "PublicDomain", Cabal.PublicDomain <$ Dhall.unit )
        , ( "AllRightsReserved", Cabal.AllRightsReserved<$ Dhall.unit )
        , ( "Unspecified", Cabal.UnspecifiedLicense <$ Dhall.unit )
        , ( "Other", Cabal.OtherLicense <$ Dhall.unit )
        ]
    )



compiler :: Dhall.Type ( Cabal.CompilerFlavor, Cabal.VersionRange )
compiler =
  makeRecord $
    (,)
      <$> keyValue "compiler" compilerFlavor
      <*> keyValue "version" versionRange



compilerFlavor :: Dhall.Type Cabal.CompilerFlavor
compilerFlavor =
  makeUnion
    ( Map.fromList
        [ ( "GHC", Cabal.GHC <$ Dhall.unit )
        , ( "GHCJS", Cabal.GHCJS <$ Dhall.unit )
        , ( "NHC", Cabal.NHC <$ Dhall.unit )
        , ( "YHC", Cabal.YHC <$ Dhall.unit )
        , ( "Hugs", Cabal.Hugs <$ Dhall.unit )
        , ( "HBC", Cabal.HBC <$ Dhall.unit )
        , ( "Helium", Cabal.Helium <$ Dhall.unit )
        , ( "JHC", Cabal.JHC <$ Dhall.unit )
        , ( "LHC", Cabal.LHC <$ Dhall.unit )
        , ( "UHC", Cabal.UHC <$ Dhall.unit )
        ]
    )



repoType :: Dhall.Type Cabal.RepoType
repoType =
  makeUnion
    ( Map.fromList
        [ ( "Git", Cabal.Git <$ Dhall.unit ) ]
    )



legacyExeDependency :: Dhall.Type Cabal.LegacyExeDependency
legacyExeDependency =
  makeRecord $ do
    exe <-
      keyValue "exe" Dhall.string

    version <-
      keyValue "version" versionRange

    pure ( Cabal.LegacyExeDependency exe version )



compilerOptions :: Dhall.Type [ ( Cabal.CompilerFlavor, [ String ] ) ]
compilerOptions =
  makeRecord $
    sequenceA
      [ (,) <$> pure Cabal.GHC <*> keyValue "GHC" options
      , (,) <$> pure Cabal.GHCJS <*> keyValue "GHCJS" options
      , (,) <$> pure Cabal.NHC <*> keyValue "NHC" options
      , (,) <$> pure Cabal.YHC <*> keyValue "YHC" options
      , (,) <$> pure Cabal.Hugs <*> keyValue "Hugs" options
      , (,) <$> pure Cabal.HBC <*> keyValue "HBC" options
      , (,) <$> pure Cabal.Helium <*> keyValue "Helium" options
      , (,) <$> pure Cabal.JHC <*> keyValue "JHC" options
      , (,) <$> pure Cabal.LHC <*> keyValue "LHC" options
      , (,) <$> pure Cabal.UHC <*> keyValue "UHC" options
      ]

  where

    options =
      Dhall.list Dhall.string



exeDependency :: Dhall.Type Cabal.ExeDependency
exeDependency =
  makeRecord $ do
    packageName <-
      keyValue "package" packageName

    component <-
      keyValue "component" unqualComponentName

    version <-
      keyValue "version" versionRange

    pure ( Cabal.ExeDependency packageName component version )



language :: Dhall.Type Cabal.Language
language =
  makeUnion
    ( Map.fromList
        [ ( "Haskell98", Cabal.Haskell98 <$ Dhall.unit  )
        , ( "Haskell2010", Cabal.Haskell2010 <$ Dhall.unit )
        ]
    )



pkgconfigDependency :: Dhall.Type Cabal.PkgconfigDependency
pkgconfigDependency =
  makeRecord $ do
    name <-
      keyValue "name" pkgconfigName

    version <-
      keyValue "version" versionRange

    return ( Cabal.PkgconfigDependency name version )



pkgconfigName :: Dhall.Type Cabal.PkgconfigName
pkgconfigName =
  Cabal.mkPkgconfigName <$> Dhall.string



executableScope :: Dhall.Type Cabal.ExecutableScope
executableScope =
  makeUnion
    ( Map.fromList
        [ ( "Public", Cabal.ExecutablePublic <$ Dhall.unit )
        , ( "Private", Cabal.ExecutablePrivate <$ Dhall.unit )
        ]
    )



moduleReexport :: Dhall.Type Cabal.ModuleReexport
moduleReexport =
  makeRecord $ do
    original <-
      keyValue "original" $
      makeRecord $ do
        package <-
          keyValue "package" ( Dhall.maybe packageName )

        name <-
          keyValue "name" moduleName

        pure ( package, name )

    moduleReexportName <-
      keyValue "name" moduleName

    pure
      Cabal.ModuleReexport
        { moduleReexportOriginalPackage = fst original
        , moduleReexportOriginalName = snd original
        , ..
        }


foreignLibOption :: Dhall.Type Cabal.ForeignLibOption
foreignLibOption =
  makeUnion
    ( Map.fromList
        [ ( "Standalone", Cabal.ForeignLibStandalone <$ Dhall.unit ) ]
    )


versionInfo :: Dhall.Type Cabal.LibVersionInfo
versionInfo =
  makeRecord $
  fmap Cabal.mkLibVersionInfo $
    (,,)
      <$> ( fromIntegral <$> keyValue "current" Dhall.natural )
      <*> ( fromIntegral <$> keyValue "revision" Dhall.natural )
      <*> ( fromIntegral <$> keyValue "age" Dhall.natural )



extension :: Dhall.Type Cabal.Extension
extension =
  Cabal.EnableExtension <$>
  makeUnion
    ( Map.fromList
        [ ( "AllowAmbiguousTypes", Cabal.AllowAmbiguousTypes <$ Dhall.unit )
        , ( "ApplicativeDo", Cabal.ApplicativeDo <$ Dhall.unit )
        , ( "Arrows", Cabal.Arrows <$ Dhall.unit )
        , ( "AutoDeriveTypeable", Cabal.AutoDeriveTypeable <$ Dhall.unit )
        , ( "BangPatterns", Cabal.BangPatterns <$ Dhall.unit )
        , ( "BinaryLiterals", Cabal.BinaryLiterals <$ Dhall.unit )
        , ( "CApiFFI", Cabal.CApiFFI <$ Dhall.unit )
        , ( "CPP", Cabal.CPP <$ Dhall.unit )
        , ( "ConstrainedClassMethods", Cabal.ConstrainedClassMethods <$ Dhall.unit )
        , ( "ConstraintKinds", Cabal.ConstraintKinds <$ Dhall.unit )
        , ( "DataKinds", Cabal.DataKinds <$ Dhall.unit )
        , ( "DatatypeContexts", Cabal.DatatypeContexts <$ Dhall.unit )
        , ( "DefaultSignatures", Cabal.DefaultSignatures <$ Dhall.unit )
        , ( "DeriveAnyClass", Cabal.DeriveAnyClass <$ Dhall.unit )
        , ( "DeriveDataTypeable", Cabal.DeriveDataTypeable <$ Dhall.unit )
        , ( "DeriveFoldable", Cabal.DeriveFoldable <$ Dhall.unit )
        , ( "DeriveFunctor", Cabal.DeriveFunctor <$ Dhall.unit )
        , ( "DeriveGeneric", Cabal.DeriveGeneric <$ Dhall.unit )
        , ( "DeriveLift", Cabal.DeriveLift <$ Dhall.unit )
        , ( "DeriveTraversable", Cabal.DeriveTraversable <$ Dhall.unit )
        , ( "DisambiguateRecordFields", Cabal.DisambiguateRecordFields <$ Dhall.unit )
        , ( "DoAndIfThenElse", Cabal.DoAndIfThenElse <$ Dhall.unit )
        , ( "DoRec", Cabal.DoRec <$ Dhall.unit )
        , ( "DuplicateRecordFields", Cabal.DuplicateRecordFields <$ Dhall.unit )
        , ( "EmptyCase", Cabal.EmptyCase <$ Dhall.unit )
        , ( "EmptyDataDecls", Cabal.EmptyDataDecls <$ Dhall.unit )
        , ( "ExistentialQuantification", Cabal.ExistentialQuantification <$ Dhall.unit )
        , ( "ExplicitForAll", Cabal.ExplicitForAll <$ Dhall.unit )
        , ( "ExplicitNamespaces", Cabal.ExplicitNamespaces <$ Dhall.unit )
        , ( "ExtendedDefaultRules", Cabal.ExtendedDefaultRules <$ Dhall.unit )
        , ( "ExtensibleRecords", Cabal.ExtensibleRecords <$ Dhall.unit )
        , ( "FlexibleContexts", Cabal.FlexibleContexts <$ Dhall.unit )
        , ( "FlexibleInstances", Cabal.FlexibleInstances <$ Dhall.unit )
        , ( "ForeignFunctionInterface", Cabal.ForeignFunctionInterface <$ Dhall.unit )
        , ( "FunctionalDependencies", Cabal.FunctionalDependencies <$ Dhall.unit )
        , ( "GADTSyntax", Cabal.GADTSyntax <$ Dhall.unit )
        , ( "GADTs", Cabal.GADTs <$ Dhall.unit )
        , ( "GHCForeignImportPrim", Cabal.GHCForeignImportPrim <$ Dhall.unit )
        , ( "GeneralizedNewtypeDeriving", Cabal.GeneralizedNewtypeDeriving <$ Dhall.unit )
        , ( "Generics", Cabal.Generics <$ Dhall.unit )
        , ( "HereDocuments", Cabal.HereDocuments <$ Dhall.unit )
        , ( "ImplicitParams", Cabal.ImplicitParams <$ Dhall.unit )
        , ( "ImplicitPrelude", Cabal.ImplicitPrelude <$ Dhall.unit )
        , ( "ImpredicativeTypes", Cabal.ImpredicativeTypes <$ Dhall.unit )
        , ( "IncoherentInstances", Cabal.IncoherentInstances <$ Dhall.unit )
        , ( "InstanceSigs", Cabal.InstanceSigs <$ Dhall.unit )
        , ( "InterruptibleFFI", Cabal.InterruptibleFFI <$ Dhall.unit )
        , ( "JavaScriptFFI", Cabal.JavaScriptFFI <$ Dhall.unit )
        , ( "KindSignatures", Cabal.KindSignatures <$ Dhall.unit )
        , ( "LambdaCase", Cabal.LambdaCase <$ Dhall.unit )
        , ( "LiberalTypeSynonyms", Cabal.LiberalTypeSynonyms <$ Dhall.unit )
        , ( "MagicHash", Cabal.MagicHash <$ Dhall.unit )
        , ( "MonadComprehensions", Cabal.MonadComprehensions <$ Dhall.unit )
        , ( "MonadFailDesugaring", Cabal.MonadFailDesugaring <$ Dhall.unit )
        , ( "MonoLocalBinds", Cabal.MonoLocalBinds <$ Dhall.unit )
        , ( "MonoPatBinds", Cabal.MonoPatBinds <$ Dhall.unit )
        , ( "MonomorphismRestriction", Cabal.MonomorphismRestriction <$ Dhall.unit )
        , ( "MultiParamTypeClasses", Cabal.MultiParamTypeClasses <$ Dhall.unit )
        , ( "MultiWayIf", Cabal.MultiWayIf <$ Dhall.unit )
        , ( "NPlusKPatterns", Cabal.NPlusKPatterns <$ Dhall.unit )
        , ( "NamedFieldPuns", Cabal.NamedFieldPuns <$ Dhall.unit )
        , ( "NamedWildCards", Cabal.NamedWildCards <$ Dhall.unit )
        , ( "NegativeLiterals", Cabal.NegativeLiterals <$ Dhall.unit )
        , ( "NewQualifiedOperators", Cabal.NewQualifiedOperators <$ Dhall.unit )
        , ( "NondecreasingIndentation", Cabal.NondecreasingIndentation <$ Dhall.unit )
        , ( "NullaryTypeClasses", Cabal.NullaryTypeClasses <$ Dhall.unit )
        , ( "NumDecimals", Cabal.NumDecimals <$ Dhall.unit )
        , ( "OverlappingInstances", Cabal.OverlappingInstances <$ Dhall.unit )
        , ( "OverloadedLabels", Cabal.OverloadedLabels <$ Dhall.unit )
        , ( "OverloadedLists", Cabal.OverloadedLists <$ Dhall.unit )
        , ( "OverloadedStrings", Cabal.OverloadedStrings <$ Dhall.unit )
        , ( "PackageImports", Cabal.PackageImports <$ Dhall.unit )
        , ( "ParallelArrays", Cabal.ParallelArrays <$ Dhall.unit )
        , ( "ParallelListComp", Cabal.ParallelListComp <$ Dhall.unit )
        , ( "PartialTypeSignatures", Cabal.PartialTypeSignatures <$ Dhall.unit )
        , ( "PatternGuards", Cabal.PatternGuards <$ Dhall.unit )
        , ( "PatternSignatures", Cabal.PatternSignatures <$ Dhall.unit )
        , ( "PatternSynonyms", Cabal.PatternSynonyms <$ Dhall.unit )
        , ( "PolyKinds", Cabal.PolyKinds <$ Dhall.unit )
        , ( "PolymorphicComponents", Cabal.PolymorphicComponents <$ Dhall.unit )
        , ( "PostfixOperators", Cabal.PostfixOperators <$ Dhall.unit )
        , ( "QuasiQuotes", Cabal.QuasiQuotes <$ Dhall.unit )
        , ( "Rank2Types", Cabal.Rank2Types <$ Dhall.unit )
        , ( "RankNTypes", Cabal.RankNTypes <$ Dhall.unit )
        , ( "RebindableSyntax", Cabal.RebindableSyntax <$ Dhall.unit )
        , ( "RecordPuns", Cabal.RecordPuns <$ Dhall.unit )
        , ( "RecordWildCards", Cabal.RecordWildCards <$ Dhall.unit )
        , ( "RecursiveDo", Cabal.RecursiveDo <$ Dhall.unit )
        , ( "RegularPatterns", Cabal.RegularPatterns <$ Dhall.unit )
        , ( "RelaxedPolyRec", Cabal.RelaxedPolyRec <$ Dhall.unit )
        , ( "RestrictedTypeSynonyms", Cabal.RestrictedTypeSynonyms <$ Dhall.unit )
        , ( "RoleAnnotations", Cabal.RoleAnnotations <$ Dhall.unit )
        , ( "Safe", Cabal.Safe <$ Dhall.unit )
        , ( "SafeImports", Cabal.SafeImports <$ Dhall.unit )
        , ( "ScopedTypeVariables", Cabal.ScopedTypeVariables <$ Dhall.unit )
        , ( "StandaloneDeriving", Cabal.StandaloneDeriving <$ Dhall.unit )
        , ( "StaticPointers", Cabal.StaticPointers <$ Dhall.unit )
        , ( "Strict", Cabal.Strict <$ Dhall.unit )
        , ( "StrictData", Cabal.StrictData <$ Dhall.unit )
        , ( "TemplateHaskell", Cabal.TemplateHaskell <$ Dhall.unit )
        , ( "TemplateHaskellQuotes", Cabal.TemplateHaskellQuotes <$ Dhall.unit )
        , ( "TraditionalRecordSyntax", Cabal.TraditionalRecordSyntax <$ Dhall.unit )
        , ( "TransformListComp", Cabal.TransformListComp <$ Dhall.unit )
        , ( "Trustworthy", Cabal.Trustworthy <$ Dhall.unit )
        , ( "TupleSections", Cabal.TupleSections <$ Dhall.unit )
        , ( "TypeApplications", Cabal.TypeApplications <$ Dhall.unit )
        , ( "TypeFamilies", Cabal.TypeFamilies <$ Dhall.unit )
        , ( "TypeFamilyDependencies", Cabal.TypeFamilyDependencies <$ Dhall.unit )
        , ( "TypeInType", Cabal.TypeInType <$ Dhall.unit )
        , ( "TypeOperators", Cabal.TypeOperators <$ Dhall.unit )
        , ( "TypeSynonymInstances", Cabal.TypeSynonymInstances <$ Dhall.unit )
        , ( "UnboxedTuples", Cabal.UnboxedTuples <$ Dhall.unit )
        , ( "UndecidableInstances", Cabal.UndecidableInstances <$ Dhall.unit )
        , ( "UndecidableSuperClasses", Cabal.UndecidableSuperClasses <$ Dhall.unit )
        , ( "UnicodeSyntax", Cabal.UnicodeSyntax <$ Dhall.unit )
        , ( "UnliftedFFITypes", Cabal.UnliftedFFITypes <$ Dhall.unit )
        , ( "Unsafe", Cabal.Unsafe <$ Dhall.unit )
        , ( "ViewPatterns", Cabal.ViewPatterns <$ Dhall.unit )
        , ( "XmlSyntax", Cabal.XmlSyntax <$ Dhall.unit )
        ]
    )



guarded
  :: Monoid a
  => Dhall.Type a
  -> Dhall.Type ( Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] a )
guarded t =
  fmap compileGuards
    $ Dhall.list
    $ makeRecord
    $ (,) <$> keyValue "guard" guard <*> keyValue "body" t

  where

    compileGuards =
      foldl combineCondTree emptyCondTree . map toCondNode

    emptyCondTree =
      Cabal.CondNode mempty [] []

    combineCondTree ( Cabal.CondNode a c1 xs ) ( Cabal.CondNode b c2 ys ) =
      Cabal.CondNode ( a <> b ) ( c1 <> c2 ) ( xs <> ys )

    toCondNode ( guard, a ) =
      let
        trueBranch =
          Cabal.CondNode a mempty mempty

      in
        if guard == Cabal.Lit True then
          trueBranch
        else
          Cabal.CondNode
            { condTreeData = mempty
            , condTreeConstraints = mempty
            , condTreeComponents =
                [ Cabal.CondBranch
                    { condBranchCondition = guard
                    , condBranchIfTrue = trueBranch
                    , condBranchIfFalse = Nothing
                    }
                ]
            }



guard :: Dhall.Type ( Cabal.Condition Cabal.ConfVar )
guard =
  let
    extractBody body =
      case body of
        Expr.BoolLit b ->
          return ( Cabal.Lit b )

        Expr.BoolAnd a b ->
          Cabal.CAnd <$> extractBody a <*> extractBody b

        Expr.BoolOr a b ->
          Cabal.COr <$> extractBody a <*> extractBody b

        Expr.BoolEQ a b ->
          Cabal.COr
            <$> ( Cabal.CAnd <$> extractBody a <*> extractBody b )
            <*> ( Cabal.CAnd
                    <$> ( Cabal.CNot <$> extractBody a )
                    <*> ( Cabal.CNot <$> extractBody b )
                )

        -- a != b ==> (a && !y) || (!a && y), but I think the following is
        -- cleaner.
        Expr.BoolNE a b ->
          Cabal.CNot <$> extractBody ( Expr.BoolEQ a b )

        Expr.BoolIf p a b ->
          Cabal.COr
            <$> ( Cabal.CAnd <$> extractBody p <*> extractBody a )
            <*> ( Cabal.CAnd
                    <$> ( Cabal.CNot <$> extractBody p )
                    <*> extractBody b
                )

        Expr.App ( Expr.App ( Expr.Field ( Expr.Var ( Expr.V "config" 0 ) ) "impl" ) compiler ) version ->
          Cabal.Var
            <$> ( Cabal.Impl
                    <$> Dhall.extract compilerFlavor compiler
                    <*> Dhall.extract versionRange version
                )

        Expr.App ( Expr.Field ( Expr.Var ( Expr.V "config" 0 ) ) field ) x ->
          case field of
            "os" ->
              Cabal.Var . Cabal.OS <$> Dhall.extract operatingSystem x

            "arch" ->
              Cabal.Var . Cabal.Arch <$> Dhall.extract arch x

            "flag" ->
              Cabal.Var . Cabal.Flag <$> Dhall.extract flagName x

            _ ->
              error "Unknown field"

        _ ->
          error ( "Unexpected guard expression. This is a bug, please report this! I'm stuck on: " ++ show body )

    extract expr = do
      Expr.Lam _ _ body <-
        return expr

      extractBody body

    expected =
      let
        predicate on =
          Expr.Pi "_" on Expr.Bool

      in
        predicate
          ( Expr.Record
              ( Map.fromList
                  [ ( "os", predicate ( Dhall.expected operatingSystem ) )
                  , ( "arch", predicate ( Dhall.expected arch ) )
                  , ( "flag", predicate ( Dhall.expected flagName ) )
                  , ( "impl"
                    , Expr.Pi
                        "_"
                        ( Dhall.expected compilerFlavor )
                        ( Expr.Pi "_" ( Dhall.expected versionRange ) Expr.Bool )
                    )
                  ]
              )
          )

  in Dhall.Type { .. }



genericPackageDescription :: Dhall.Type Cabal.GenericPackageDescription
genericPackageDescription =
  let
    namedList k t =
      Dhall.list
        ( makeRecord
            ( (,)
                <$> keyValue "name" unqualComponentName
                <*> keyValue k ( guarded t )
            )
        )

  in
    makeRecord $ do
      packageDescription <-
        packageDescription

      genPackageFlags <-
        keyValue "flags" ( Dhall.list flag )

      condLibrary <-
        keyValue "library" ( Dhall.maybe ( guarded library ) )

      condSubLibraries <-
        keyValue "sub-libraries" ( namedList "library" library )

      condForeignLibs <-
        keyValue "foreign-libraries" ( namedList "foreign-lib" foreignLib )

      condExecutables <-
        keyValue "executables" ( namedList "executable" executable )

      condTestSuites <-
        keyValue "test-suites" ( namedList "test-suite" testSuite )

      condBenchmarks <-
        keyValue "benchmarks" ( namedList "benchmark" benchmark )

      return Cabal.GenericPackageDescription { .. }



operatingSystem :: Dhall.Type Cabal.OS
operatingSystem =
  Dhall.genericAuto



arch :: Dhall.Type Cabal.Arch
arch =
  Dhall.genericAuto



flag :: Dhall.Type Cabal.Flag
flag =
  makeRecord $ do
    flagName <-
      keyValue "name" flagName

    flagDefault <-
      keyValue "default" Dhall.bool

    flagDescription <-
      keyValue "description" Dhall.string

    flagManual <-
      keyValue "manual" Dhall.bool

    return Cabal.MkFlag { .. }



flagName :: Dhall.Type Cabal.FlagName
flagName =
  Cabal.mkFlagName <$> Dhall.string
