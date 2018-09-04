{-# language ApplicativeDo #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module DhallToCabal
  ( dhallToCabal
  , genericPackageDescription
  , sourceRepo
  , repoKind
  , repoType
  , compiler
  , operatingSystem
  , library
  , extension
  , compilerOptions
  , guarded
  , arch
  , compilerFlavor
  , language
  , license
  , spdxLicense
  , spdxLicenseId
  , spdxLicenseExceptionId
  , executable
  , testSuite
  , benchmark
  , foreignLib
  , buildType
  , versionRange
  , version
  , configRecordType
  , buildInfoType

  , sortExpr
  ) where

import Data.List ( partition )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )

import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Data.Text as StrictText
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Distribution.Compiler as Cabal
import qualified Distribution.License as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.SPDX as SPDX
import qualified Distribution.System as Cabal ( Arch(..), OS(..) )
import qualified Distribution.Text as Cabal ( simpleParse )
import qualified Distribution.Types.CondTree as Cabal
import qualified Distribution.Types.Dependency as Cabal
import qualified Distribution.Types.ExeDependency as Cabal
import qualified Distribution.Types.ExecutableScope as Cabal
import qualified Distribution.Types.ForeignLib as Cabal
import qualified Distribution.Types.ForeignLibOption as Cabal
import qualified Distribution.Types.ForeignLibType as Cabal
import qualified Distribution.Types.IncludeRenaming as Cabal
import qualified Distribution.Types.LegacyExeDependency as Cabal
import qualified Distribution.Types.Mixin as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PkgconfigDependency as Cabal
import qualified Distribution.Types.PkgconfigName as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Version as Cabal
import qualified Language.Haskell.Extension as Cabal

import qualified Dhall.Core as Expr
  ( Chunks(..), Const(..), Expr(..), Var(..) )

import Dhall.Extra
import DhallToCabal.ConfigTree ( ConfigTree(..), toConfigTree )
import DhallToCabal.Diff ( Diffable(..)  )



packageIdentifier :: Dhall.RecordType Cabal.PackageIdentifier
packageIdentifier = do
  pkgName <-
    Dhall.field "name" packageName

  pkgVersion <-
    Dhall.field "version" version

  pure Cabal.PackageIdentifier { .. }



packageName :: Dhall.Type Cabal.PackageName
packageName =
  Cabal.mkPackageName <$> Dhall.string



packageDescription :: Dhall.RecordType Cabal.PackageDescription
packageDescription = do
  package <-
    packageIdentifier

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
    Dhall.field
      "x-fields"
      ( Dhall.list ( Dhall.pair Dhall.string Dhall.string ) )

  sourceRepos <-
    Dhall.field "source-repos" ( Dhall.list sourceRepo )

  specVersionRaw <-
    Left <$> Dhall.field "cabal-version" version

  buildTypeRaw <-
    Dhall.field "build-type" ( Dhall.maybe buildType )

  licenseRaw <-
    Dhall.field "license" license

  licenseFiles <-
    Dhall.field "license-files" ( Dhall.list Dhall.string )

  copyright <-
    Dhall.field "copyright" Dhall.string

  maintainer <-
    Dhall.field "maintainer" Dhall.string

  author <-
    Dhall.field "author" Dhall.string

  stability <-
    Dhall.field "stability" Dhall.string

  testedWith <-
    Dhall.field "tested-with" ( Dhall.list compiler )

  homepage <-
    Dhall.field "homepage" Dhall.string

  pkgUrl <-
    Dhall.field "package-url" Dhall.string

  bugReports <-
    Dhall.field "bug-reports" Dhall.string

  synopsis <-
    Dhall.field "synopsis" Dhall.string

  description <-
    Dhall.field "description" Dhall.string

  category <-
    Dhall.field "category" Dhall.string

  -- Cabal documentation states
  --
  --   > YOU PROBABLY DON'T WANT TO USE THIS FIELD.
  --
  -- So I guess we won't use this field.
  buildDepends <-
    pure []

  setupBuildInfo <-
    Dhall.field "custom-setup" ( Dhall.maybe setupBuildInfo )

  dataFiles <-
    Dhall.field "data-files" ( Dhall.list Dhall.string )

  dataDir <-
    Dhall.field "data-dir" Dhall.string

  extraSrcFiles <-
    Dhall.field "extra-source-files" ( Dhall.list Dhall.string )

  extraTmpFiles <-
    Dhall.field "extra-tmp-files" ( Dhall.list Dhall.string )

  extraDocFiles <-
    Dhall.field "extra-doc-files" ( Dhall.list Dhall.string )

  return ( adjustUnspecifiedLicense Cabal.PackageDescription { .. } )

  where
    -- Cabal reads a 2.0-spec file without a license as having
    -- SPDX.NONE, but outputting SPDX-style licenses with 2.0 doesn't
    -- work (and outputting legacy non-SPDX licenses with 2.2 also
    -- doesn't work). In CabalToDhall we map SPDX.NONE to
    -- AllRightsReserved, but we still have to switch between
    -- SPDX-style NONE and legacy-style AllRightsReserved here
    -- depending on cabal-version.
    adjustUnspecifiedLicense desc
      | Cabal.specVersion desc >= Cabal.mkVersion [2,2]
      , Cabal.licenseRaw desc == Right Cabal.AllRightsReserved
        = desc { Cabal.licenseRaw = Left SPDX.NONE }
      | otherwise
        = desc


version :: Dhall.Type Cabal.Version
version =
  let
    parse text =
      fromMaybe
        ( error "Could not parse version" )
        ( Cabal.simpleParse ( StrictText.unpack text ) )

    extract e =
      go
        ( Dhall.Core.normalize ( e `Expr.App` "Version" `Expr.App` "v" )
            `asTypeOf` e
        )

    go =
      \case
        Expr.App "v" ( Expr.TextLit ( Expr.Chunks [] text ) ) ->
          return ( parse text )

        e ->
          error ( show e )

    expected =
      Expr.Pi "Version" ( Expr.Const Expr.Type )
        $ Expr.Pi
            "v"
            ( Expr.Pi "_" ( Dhall.expected Dhall.string ) "Version" )
            "Version"

  in Dhall.Type { .. }



benchmark :: Dhall.Type Cabal.Benchmark
benchmark =
  Dhall.record $ do
    mainIs <-
      Dhall.field "main-is" Dhall.string

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



buildInfo :: Dhall.RecordType Cabal.BuildInfo
buildInfo = do
  buildable <-
    Dhall.field "buildable" Dhall.bool

  buildTools <-
    Dhall.field "build-tools" ( Dhall.list legacyExeDependency )

  buildToolDepends <-
    Dhall.field "build-tool-depends" ( Dhall.list exeDependency )

  cppOptions <-
    Dhall.field "cpp-options" ( Dhall.list Dhall.string )

  ccOptions <-
    Dhall.field "cc-options" ( Dhall.list Dhall.string )

  ldOptions <-
    Dhall.field "ld-options" ( Dhall.list Dhall.string )

  pkgconfigDepends <-
    Dhall.field "pkgconfig-depends" ( Dhall.list pkgconfigDependency )

  frameworks <-
    Dhall.field "frameworks" ( Dhall.list Dhall.string )

  extraFrameworkDirs <-
    Dhall.field "extra-framework-dirs" ( Dhall.list Dhall.string )

  cSources <-
    Dhall.field "c-sources" ( Dhall.list Dhall.string )

  jsSources <-
    Dhall.field "js-sources" ( Dhall.list Dhall.string )

  hsSourceDirs <-
    Dhall.field "hs-source-dirs" ( Dhall.list Dhall.string )

  otherModules <-
    Dhall.field "other-modules" ( Dhall.list moduleName )

  autogenModules <-
    Dhall.field "autogen-modules" ( Dhall.list moduleName )

  defaultLanguage <-
    Dhall.field "default-language" ( Dhall.maybe language )

  otherLanguages <-
    Dhall.field "other-languages" ( Dhall.list language )

  defaultExtensions <-
    Dhall.field "default-extensions" ( Dhall.list extension )

  otherExtensions <-
    Dhall.field "other-extensions" ( Dhall.list extension )

  oldExtensions <-
    pure []

  extraLibs <-
    Dhall.field "extra-libraries" ( Dhall.list Dhall.string )

  extraGHCiLibs <-
    Dhall.field "extra-ghci-libraries" ( Dhall.list Dhall.string )

  extraLibDirs <-
    Dhall.field "extra-lib-dirs" ( Dhall.list Dhall.string )

  includeDirs <-
    Dhall.field "include-dirs" ( Dhall.list Dhall.string )

  includes <-
    Dhall.field "includes" ( Dhall.list Dhall.string )

  installIncludes <-
    Dhall.field "install-includes" ( Dhall.list Dhall.string )

  options <-
    Dhall.field "compiler-options" compilerOptions

  profOptions <-
    Dhall.field "profiling-options" compilerOptions

  sharedOptions <-
    Dhall.field "shared-options" compilerOptions

  staticOptions <-
    Dhall.field "static-options" compilerOptions

  customFieldsBI <-
    pure []

  targetBuildDepends <-
    Dhall.field "build-depends" ( Dhall.list dependency )

  mixins <-
    Dhall.field "mixins" ( Dhall.list mixin )

  asmOptions <-
    Dhall.field "asm-options" ( Dhall.list Dhall.string )

  asmSources <-
    Dhall.field "asm-sources" ( Dhall.list Dhall.string )

  cmmOptions <-
    Dhall.field "cmm-options" ( Dhall.list Dhall.string )

  cmmSources <-
    Dhall.field "cmm-sources" ( Dhall.list Dhall.string )

  cxxOptions <-
    Dhall.field "cxx-options" ( Dhall.list Dhall.string )

  cxxSources <-
    Dhall.field "cxx-sources" ( Dhall.list Dhall.string )

  virtualModules <-
    Dhall.field "virtual-modules" ( Dhall.list moduleName )

  extraLibFlavours <-
    Dhall.field "extra-lib-flavours" ( Dhall.list Dhall.string )

  extraBundledLibs <-
    Dhall.field "extra-bundled-libs" ( Dhall.list Dhall.string )

  return Cabal.BuildInfo { ..  }


buildInfoType :: Expr.Expr Dhall.Parser.Src Dhall.TypeCheck.X
buildInfoType =
  Dhall.expected ( Dhall.record buildInfo )


testSuite :: Dhall.Type Cabal.TestSuite
testSuite =
  Dhall.record $ do
    testName <-
      pure ""

    testBuildInfo <-
      buildInfo

    testInterface <-
      Dhall.field "type" testSuiteInterface

    pure
      Cabal.TestSuite
        { ..
        }



testSuiteInterface :: Dhall.Type Cabal.TestSuiteInterface
testSuiteInterface =
  makeUnion
    ( Map.fromList
        [ ( "exitcode-stdio"
          , Cabal.TestSuiteExeV10 ( Cabal.mkVersion [ 1, 0 ] )
              <$> Dhall.record ( Dhall.field "main-is" Dhall.string )
          )
        , ( "detailed"
          , Cabal.TestSuiteLibV09 ( Cabal.mkVersion [ 0, 9 ] )
              <$> Dhall.record ( Dhall.field "module" moduleName )
          )
        ]
    )




unqualComponentName :: Dhall.Type Cabal.UnqualComponentName
unqualComponentName =
  Cabal.mkUnqualComponentName <$> Dhall.string



executable :: Dhall.Type Cabal.Executable
executable =
  Dhall.record $ do
    exeName <-
      pure ""

    modulePath <-
      Dhall.field "main-is" Dhall.string

    exeScope <-
      Dhall.field "scope" executableScope

    buildInfo <-
      buildInfo

    pure Cabal.Executable { .. }



foreignLib :: Dhall.Type Cabal.ForeignLib
foreignLib =
  Dhall.record $ do
    foreignLibName <-
      pure ""

    foreignLibType <-
      Dhall.field "type" foreignLibType

    foreignLibOptions <-
      Dhall.field "options" ( Dhall.list foreignLibOption )

    foreignLibBuildInfo <-
      buildInfo

    foreignLibVersionInfo <-
      Dhall.field "lib-version-info" ( Dhall.maybe versionInfo )

    foreignLibVersionLinux <-
      Dhall.field "lib-version-linux" ( Dhall.maybe version )

    foreignLibModDefFile <-
      Dhall.field "mod-def-files" ( Dhall.list Dhall.string )

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
  Dhall.record $ do
    libName <-
      pure Nothing

    libBuildInfo <-
      buildInfo

    exposedModules <-
      Dhall.field "exposed-modules" ( Dhall.list moduleName )

    reexportedModules <-
      Dhall.field "reexported-modules" ( Dhall.list moduleReexport )

    signatures <-
      Dhall.field "signatures" ( Dhall.list moduleName )

    libExposed <-
      pure True

    pure Cabal.Library { .. }



sourceRepo :: Dhall.Type Cabal.SourceRepo
sourceRepo =
  Dhall.record $ do
    repoKind <-
      Dhall.field "kind" repoKind

    repoType <-
      Dhall.field "type" ( Dhall.maybe repoType )

    repoLocation <-
      Dhall.field "location" ( Dhall.maybe Dhall.string )

    repoModule <-
      Dhall.field "module" ( Dhall.maybe Dhall.string )

    repoBranch <-
      Dhall.field "branch" ( Dhall.maybe Dhall.string )

    repoTag <-
      Dhall.field "tag" ( Dhall.maybe Dhall.string )

    repoSubdir <-
      Dhall.field "subdir" ( Dhall.maybe filePath )

    pure Cabal.SourceRepo { .. }



repoKind :: Dhall.Type Cabal.RepoKind
repoKind =
  sortType Dhall.genericAuto



dependency :: Dhall.Type Cabal.Dependency
dependency =
  Dhall.record $ do
    packageName <-
      Dhall.field "package" packageName

    versionRange <-
      Dhall.field "bounds" versionRange

    pure ( Cabal.Dependency packageName versionRange )



moduleName :: Dhall.Type Cabal.ModuleName
moduleName =
  validateType $
    Cabal.simpleParse <$> Dhall.string



dhallToCabal
  :: Dhall.InputSettings
  -> StrictText.Text
  -- ^ The Dhall to parse.
  -> IO Cabal.GenericPackageDescription
dhallToCabal settings =
  Dhall.inputWithSettings settings genericPackageDescription



versionRange :: Dhall.Type Cabal.VersionRange
versionRange =
  let
    extract e =
      go
        ( Dhall.Core.normalize
            ( e
                `Expr.App` "VersionRange"
                `Expr.App` "anyVersion"
                `Expr.App` "noVersion"
                `Expr.App` "thisVersion"
                `Expr.App` "notThisVersion"
                `Expr.App` "laterVersion"
                `Expr.App` "earlierVersion"
                `Expr.App` "orLaterVersion"
                `Expr.App` "orEarlierVersion"
                `Expr.App` "withinVersion"
                `Expr.App` "majorBoundVersion"
                `Expr.App` "unionVersionRanges"
                `Expr.App` "intersectVersionRanges"
                `Expr.App` "differenceVersionRanges"
                `Expr.App` "invertVersionRange"
            )
            `asTypeOf` e
        )

    go =
      \case
        "anyVersion" ->
          return Cabal.anyVersion

        "noVersion" ->
          return Cabal.noVersion

        Expr.App "thisVersion" components ->
          Cabal.thisVersion <$> Dhall.extract version components

        Expr.App "notThisVersion" components ->
          Cabal.notThisVersion <$> Dhall.extract version components

        Expr.App "laterVersion" components ->
          Cabal.laterVersion <$> Dhall.extract version components

        Expr.App "earlierVersion" components ->
          Cabal.earlierVersion <$> Dhall.extract version components

        Expr.App "orLaterVersion" components ->
          Cabal.orLaterVersion <$> Dhall.extract version components

        Expr.App "orEarlierVersion" components ->
          Cabal.orEarlierVersion <$> Dhall.extract version components

        Expr.App ( Expr.App "unionVersionRanges" a ) b ->
          Cabal.unionVersionRanges <$> go a <*> go b

        Expr.App ( Expr.App "intersectVersionRanges" a ) b ->
          Cabal.intersectVersionRanges <$> go a <*> go b

        Expr.App ( Expr.App "differenceVersionRanges" a ) b ->
          Cabal.differenceVersionRanges <$> go a <*> go b

        Expr.App "invertVersionRange" components ->
          Cabal.invertVersionRange <$> go components

        Expr.App "withinVersion" components ->
          Cabal.withinVersion <$> Dhall.extract version components

        Expr.App "majorBoundVersion" components ->
          Cabal.majorBoundVersion <$> Dhall.extract version components

        _ ->
          Nothing

    expected =
      let
        versionRange =
          "VersionRange"

        versionToVersionRange =
          Expr.Pi
            "_"
            ( Dhall.expected version )
            versionRange

        combine =
          Expr.Pi "_" versionRange ( Expr.Pi "_" versionRange versionRange )

      in
      Expr.Pi "VersionRange" ( Expr.Const Expr.Type )
        $ Expr.Pi "anyVersion" versionRange
        $ Expr.Pi "noVersion" versionRange
        $ Expr.Pi "thisVersion" versionToVersionRange
        $ Expr.Pi "notThisVersion" versionToVersionRange
        $ Expr.Pi "laterVersion" versionToVersionRange
        $ Expr.Pi "earlierVersion" versionToVersionRange
        $ Expr.Pi "orLaterVersion" versionToVersionRange
        $ Expr.Pi "orEarlierVersion" versionToVersionRange
        $ Expr.Pi "withinVersion" versionToVersionRange
        $ Expr.Pi "majorBoundVersion" versionToVersionRange
        $ Expr.Pi "unionVersionRanges" combine
        $ Expr.Pi "intersectVersionRanges" combine
        $ Expr.Pi "differenceVersionRanges" combine
        $ Expr.Pi
            "invertVersionRange"
            ( Expr.Pi "_" versionRange versionRange )
            versionRange

  in Dhall.Type { .. }



buildType :: Dhall.Type Cabal.BuildType
buildType =
  sortType Dhall.genericAuto



license :: Dhall.Type (Either SPDX.License Cabal.License)
license =
  makeUnion
    ( Map.fromList
        [ ( "GPL", Right . Cabal.GPL <$> Dhall.maybe version )
        , ( "AGPL", Right . Cabal.AGPL <$> Dhall.maybe version )
        , ( "LGPL", Right . Cabal.LGPL <$> Dhall.maybe version )
        , ( "BSD2", Right Cabal.BSD2 <$ Dhall.unit )
        , ( "BSD3", Right Cabal.BSD3 <$ Dhall.unit )
        , ( "BSD4", Right Cabal.BSD4 <$ Dhall.unit )
        , ( "MIT", Right Cabal.MIT <$ Dhall.unit )
        , ( "ISC", Right Cabal.ISC <$ Dhall.unit )
        , ( "MPL", Right . Cabal.MPL <$> version )
        , ( "Apache", Right . Cabal.Apache <$> Dhall.maybe version )
        , ( "PublicDomain", Right Cabal.PublicDomain <$ Dhall.unit )
        , ( "AllRightsReserved", Right Cabal.AllRightsReserved<$ Dhall.unit )
        , ( "Unspecified", Right Cabal.UnspecifiedLicense <$ Dhall.unit )
        , ( "Other", Right Cabal.OtherLicense <$ Dhall.unit )
        , ( "SPDX", Left . SPDX.License <$> spdxLicense )
        ]
    )


spdxLicense :: Dhall.Type SPDX.LicenseExpression
spdxLicense =
  let
    extract e =
      go
        ( Dhall.Core.normalize
            ( e
                `Expr.App` "SPDX"
                `Expr.App` "license"
                `Expr.App` "licenseVersionOrLater"
                `Expr.App` "ref"
                `Expr.App` "refWithFile"
                `Expr.App` "and"
                `Expr.App` "or"
            )
            `asTypeOf` e
        )

    go =
      \case
        Expr.App ( Expr.App "license" identM ) exceptionMayM -> do
          ident <- Dhall.extract spdxLicenseId identM
          exceptionMay <- Dhall.extract ( Dhall.maybe spdxLicenseExceptionId ) exceptionMayM
          return ( SPDX.ELicense ( SPDX.ELicenseId ident ) exceptionMay )

        Expr.App ( Expr.App "licenseVersionOrLater" identM ) exceptionMayM -> do
          ident <- Dhall.extract spdxLicenseId identM
          exceptionMay <- Dhall.extract ( Dhall.maybe spdxLicenseExceptionId ) exceptionMayM
          return ( SPDX.ELicense ( SPDX.ELicenseIdPlus ident ) exceptionMay )

        Expr.App ( Expr.App "ref" identM ) exceptionMayM -> do
          ident <- Dhall.extract Dhall.string identM
          exceptionMay <- Dhall.extract ( Dhall.maybe spdxLicenseExceptionId ) exceptionMayM
          return ( SPDX.ELicense ( SPDX.ELicenseRef ( SPDX.mkLicenseRef' Nothing ident ) ) exceptionMay )

        Expr.App ( Expr.App ( Expr.App "refWithFile" identM ) filenameM) exceptionMayM -> do
          ident <- Dhall.extract Dhall.string identM
          filename <- Dhall.extract Dhall.string filenameM
          exceptionMay <- Dhall.extract ( Dhall.maybe spdxLicenseExceptionId ) exceptionMayM
          return ( SPDX.ELicense ( SPDX.ELicenseRef ( SPDX.mkLicenseRef' ( Just filename ) ident ) ) exceptionMay )

        Expr.App ( Expr.App "and" a ) b ->
          SPDX.EAnd <$> go a <*> go b

        Expr.App ( Expr.App "or" a ) b ->
          SPDX.EOr <$> go a <*> go b

        _ ->
          Nothing

    expected =
      let
        licenseType =
          "SPDX"

        licenseIdAndException
          = Expr.Pi "id" ( Dhall.expected spdxLicenseId )
          $ Expr.Pi "exception" ( Dhall.expected ( Dhall.maybe spdxLicenseExceptionId ) )
          $ licenseType

        licenseRef
          = Expr.Pi "ref" ( Dhall.expected Dhall.string )
          $ Expr.Pi "exception" ( Dhall.expected ( Dhall.maybe spdxLicenseExceptionId ) )
          $ licenseType

        licenseRefWithFile
          = Expr.Pi "ref" ( Dhall.expected Dhall.string )
          $ Expr.Pi "file" ( Dhall.expected Dhall.string )
          $ Expr.Pi "exception" ( Dhall.expected ( Dhall.maybe spdxLicenseExceptionId ) )
          $ licenseType

        combine =
          Expr.Pi "_" licenseType ( Expr.Pi "_" licenseType licenseType )

      in
      Expr.Pi "SPDX" ( Expr.Const Expr.Type )
        $ Expr.Pi "license" licenseIdAndException
        $ Expr.Pi "licenseVersionOrLater" licenseIdAndException
        $ Expr.Pi "ref" licenseRef
        $ Expr.Pi "refWithFile" licenseRefWithFile
        $ Expr.Pi "and" combine
        $ Expr.Pi "or" combine
        $ licenseType

  in Dhall.Type { .. }



spdxLicenseId :: Dhall.Type SPDX.LicenseId
spdxLicenseId = Dhall.genericAuto



spdxLicenseExceptionId :: Dhall.Type SPDX.LicenseExceptionId
spdxLicenseExceptionId = Dhall.genericAuto



compiler :: Dhall.Type ( Cabal.CompilerFlavor, Cabal.VersionRange )
compiler =
  Dhall.record $
    (,)
      <$> Dhall.field "compiler" compilerFlavor
      <*> Dhall.field "version" versionRange



compilerFlavor :: Dhall.Type Cabal.CompilerFlavor
compilerFlavor =
  sortType Dhall.genericAuto



repoType :: Dhall.Type Cabal.RepoType
repoType =
  sortType Dhall.genericAuto



legacyExeDependency :: Dhall.Type Cabal.LegacyExeDependency
legacyExeDependency =
  Dhall.record $ do
    exe <-
      Dhall.field "exe" Dhall.string

    version <-
      Dhall.field "version" versionRange

    pure ( Cabal.LegacyExeDependency exe version )



compilerOptions :: Dhall.Type [ ( Cabal.CompilerFlavor, [ String ] ) ]
compilerOptions =
  Dhall.record $
    sequenceA
      [ (,) <$> pure Cabal.GHC <*> Dhall.field "GHC" options
      , (,) <$> pure Cabal.GHCJS <*> Dhall.field "GHCJS" options
      , (,) <$> pure Cabal.NHC <*> Dhall.field "NHC" options
      , (,) <$> pure Cabal.YHC <*> Dhall.field "YHC" options
      , (,) <$> pure Cabal.Hugs <*> Dhall.field "Hugs" options
      , (,) <$> pure Cabal.HBC <*> Dhall.field "HBC" options
      , (,) <$> pure Cabal.Helium <*> Dhall.field "Helium" options
      , (,) <$> pure Cabal.JHC <*> Dhall.field "JHC" options
      , (,) <$> pure Cabal.LHC <*> Dhall.field "LHC" options
      , (,) <$> pure Cabal.UHC <*> Dhall.field "UHC" options
      ]

  where

    options =
      Dhall.list Dhall.string



exeDependency :: Dhall.Type Cabal.ExeDependency
exeDependency =
  Dhall.record $ do
    packageName <-
      Dhall.field "package" packageName

    component <-
      Dhall.field "component" unqualComponentName

    version <-
      Dhall.field "version" versionRange

    pure ( Cabal.ExeDependency packageName component version )



language :: Dhall.Type Cabal.Language
language =
  sortType Dhall.genericAuto



pkgconfigDependency :: Dhall.Type Cabal.PkgconfigDependency
pkgconfigDependency =
  Dhall.record $ do
    name <-
      Dhall.field "name" pkgconfigName

    version <-
      Dhall.field "version" versionRange

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
  Dhall.record $ do
    original <-
      Dhall.field "original" $
      Dhall.record $ do
        package <-
          Dhall.field "package" ( Dhall.maybe packageName )

        name <-
          Dhall.field "name" moduleName

        pure ( package, name )

    moduleReexportName <-
      Dhall.field "name" moduleName

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
  Dhall.record $
  fmap Cabal.mkLibVersionInfo $
    (,,)
      <$> ( fromIntegral <$> Dhall.field "current" Dhall.natural )
      <*> ( fromIntegral <$> Dhall.field "revision" Dhall.natural )
      <*> ( fromIntegral <$> Dhall.field "age" Dhall.natural )



extension :: Dhall.Type Cabal.Extension
extension =
  let
    knownExtension =
      sortType Dhall.genericAuto

    unitType =
      Expr.Record Map.empty

    extract expr = do
      Expr.UnionLit k v alts <-
        return expr

      ext <-
        Dhall.extract
          knownExtension
          ( Expr.UnionLit k ( Expr.RecordLit mempty ) ( unitType <$ alts ) )

      case v of
        Expr.BoolLit True ->
          return ( Cabal.EnableExtension ext )

        Expr.BoolLit False ->
          return ( Cabal.DisableExtension ext )

        _ ->
          Nothing

    expected =
      case Dhall.expected knownExtension of
        Expr.Union alts ->
          sortExpr ( Expr.Union ( Expr.Bool <$ alts ) )

        _ ->
          error "Could not derive extension type"

  in Dhall.Type { .. }



guarded
  :: ( Monoid a, Eq a, Diffable a )
  => Dhall.Type a
  -> Dhall.Type ( Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] a )
guarded t =
  let
    extractConfVar body =
      case body of
        Expr.App ( Expr.App ( Expr.Field "config" "impl" ) compiler ) version ->
          Cabal.Impl
            <$> Dhall.extract compilerFlavor compiler
            <*> Dhall.extract versionRange version

        Expr.App ( Expr.Field "config" field ) x ->
          case field of
            "os" ->
              Cabal.OS <$> Dhall.extract operatingSystem x

            "arch" ->
              Cabal.Arch <$> Dhall.extract arch x

            "flag" ->
              Cabal.Flag <$> Dhall.extract flagName x

            _ ->
              error "Unknown field"

        _ ->
          error ( "Unexpected guard expression. This is a bug, please report this! I'm stuck on: " ++ show body )

    extract expr =
      configTreeToCondTree [] [] <$> extractConfigTree ( toConfigTree expr )

    extractConfigTree ( Leaf a ) =
      Leaf <$> Dhall.extract t a

    extractConfigTree ( Branch cond a b ) =
      Branch <$> extractConfVar cond <*> extractConfigTree a <*> extractConfigTree b

    configTreeToCondTree confVarsTrue confVarsFalse = \case
      Leaf a ->
        Cabal.CondNode a mempty mempty

      -- The condition has already been shown to hold. Consider only the true
      -- branch and discard the false branch.
      Branch confVar a _impossible | confVar `elem` confVarsTrue ->
        configTreeToCondTree confVarsTrue confVarsFalse a

      -- ...and here, the condition has been shown *not* to hold.
      Branch confVar _impossible b | confVar `elem` confVarsFalse ->
        configTreeToCondTree confVarsTrue confVarsFalse b

      Branch confVar a b ->
        let
          true =
            configTreeToCondTree ( pure confVar <> confVarsTrue ) confVarsFalse a

          false =
            configTreeToCondTree confVarsTrue ( pure confVar <> confVarsFalse ) b

          ( common, true', false' ) =
            diff ( Cabal.condTreeData true ) ( Cabal.condTreeData false )

          ( duplicates, true'', false'' ) =
            diff
              ( Cabal.condTreeComponents true )
              ( Cabal.condTreeComponents false )

        in
          Cabal.CondNode
            common
            mempty
            ( mergeCommonGuards
                ( Cabal.CondBranch
                    ( Cabal.Var confVar )
                    true
                      { Cabal.condTreeData = true'
                      , Cabal.condTreeComponents = true''
                      }
                    ( Just
                        false
                          { Cabal.condTreeData = false'
                          , Cabal.condTreeComponents = false''
                          }
                    )
                : duplicates
                )
            )

    expected =
        Expr.Pi "_" configRecordType ( Dhall.expected t )

  in Dhall.Type { .. }



catCondTree
  :: ( Monoid c, Monoid a )
  => Cabal.CondTree v c a -> Cabal.CondTree v c a -> Cabal.CondTree v c a
catCondTree a b =
  Cabal.CondNode
    { Cabal.condTreeData =
        Cabal.condTreeData a <> Cabal.condTreeData b
    , Cabal.condTreeConstraints =
        Cabal.condTreeConstraints a <> Cabal.condTreeConstraints b
    , Cabal.condTreeComponents =
        Cabal.condTreeComponents a <> Cabal.condTreeComponents b
    }



emptyCondTree :: ( Monoid b, Monoid c ) => Cabal.CondTree a b c
emptyCondTree =
  Cabal.CondNode mempty mempty mempty



mergeCommonGuards
  :: ( Monoid a, Monoid c, Eq v )
  => [Cabal.CondBranch v c a]
  -> [Cabal.CondBranch v c a]
mergeCommonGuards [] =
  []

mergeCommonGuards ( a : as ) =
  let
    ( sameGuard, differentGuard ) =
      partition
        ( ( Cabal.condBranchCondition a == ) . Cabal.condBranchCondition )
        as

  in
    a
      { Cabal.condBranchIfTrue =
          catCondTree
            ( Cabal.condBranchIfTrue a )
            ( foldl
                catCondTree
                emptyCondTree
                ( Cabal.condBranchIfTrue <$> sameGuard )
            )
      , Cabal.condBranchIfFalse =
          Just
            ( catCondTree
              ( fromMaybe emptyCondTree ( Cabal.condBranchIfFalse a ) )
              ( foldl
                  catCondTree
                  emptyCondTree
                  ( fromMaybe emptyCondTree
                      . Cabal.condBranchIfFalse
                      <$> sameGuard
                  )
              )
            )
      }
      : mergeCommonGuards differentGuard



configRecordType :: Expr.Expr Dhall.Parser.Src Dhall.TypeCheck.X
configRecordType =
  let
    predicate on =
      Expr.Pi "_" on Expr.Bool

  in
    Expr.Record
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



genericPackageDescription :: Dhall.Type Cabal.GenericPackageDescription
genericPackageDescription =
  let
    namedList k t =
      Dhall.list
        ( Dhall.record
            ( (,)
                <$> Dhall.field "name" unqualComponentName
                <*> Dhall.field k ( guarded t )
            )
        )

  in
    Dhall.record $ do
      packageDescription <-
        packageDescription

      genPackageFlags <-
        Dhall.field "flags" ( Dhall.list flag )

      condLibrary <-
        Dhall.field "library" ( Dhall.maybe ( guarded library ) )

      condSubLibraries <-
        Dhall.field "sub-libraries" ( namedList "library" library )

      condForeignLibs <-
        Dhall.field "foreign-libraries" ( namedList "foreign-lib" foreignLib )

      condExecutables <-
        Dhall.field "executables" ( namedList "executable" executable )

      condTestSuites <-
        Dhall.field "test-suites" ( namedList "test-suite" testSuite )

      condBenchmarks <-
        Dhall.field "benchmarks" ( namedList "benchmark" benchmark )

      return Cabal.GenericPackageDescription { .. }



operatingSystem :: Dhall.Type Cabal.OS
operatingSystem =
  sortType Dhall.genericAuto



arch :: Dhall.Type Cabal.Arch
arch =
  sortType Dhall.genericAuto



flag :: Dhall.Type Cabal.Flag
flag =
  Dhall.record $ do
    flagName <-
      Dhall.field "name" flagName

    flagDefault <-
      Dhall.field "default" Dhall.bool

    flagDescription <-
      Dhall.field "description" Dhall.string

    flagManual <-
      Dhall.field "manual" Dhall.bool

    return Cabal.MkFlag { .. }



flagName :: Dhall.Type Cabal.FlagName
flagName =
  Cabal.mkFlagName <$> Dhall.string



setupBuildInfo :: Dhall.Type Cabal.SetupBuildInfo
setupBuildInfo =
  Dhall.record $ do
    setupDepends <-
      Dhall.field "setup-depends" ( Dhall.list dependency )

    defaultSetupDepends <-
      pure False

    return Cabal.SetupBuildInfo { .. }



filePath :: Dhall.Type FilePath
filePath =
  Dhall.string



mixin :: Dhall.Type Cabal.Mixin
mixin =
  Dhall.record $ do
    mixinPackageName <-
      Dhall.field "package" packageName

    mixinIncludeRenaming <-
      Dhall.field "renaming" includeRenaming

    pure Cabal.Mixin { .. }



includeRenaming :: Dhall.Type Cabal.IncludeRenaming
includeRenaming =
  Dhall.record $ do
    includeProvidesRn <-
      Dhall.field "provides" moduleRenaming

    includeRequiresRn <-
      Dhall.field "requires" moduleRenaming

    pure Cabal.IncludeRenaming { .. }



moduleRenaming :: Dhall.Type Cabal.ModuleRenaming
moduleRenaming =
  makeUnion
    ( Map.fromList
      [ ( "renaming"
        , fmap Cabal.ModuleRenaming
            ( Dhall.list
              ( Dhall.record
                ( (,) <$> Dhall.field "rename" moduleName <*> Dhall.field "to" moduleName )
              )
            )
        )
      , ( "default"
        , Dhall.record ( pure Cabal.DefaultRenaming )
        )
      , ( "hiding"
        , fmap Cabal.HidingRenaming
            ( Dhall.list moduleName )
        )
      ]
    )


sortType :: Dhall.Type a -> Dhall.Type a
sortType t =
  t { Dhall.expected = sortExpr ( Dhall.expected t ) }
