{-# language ApplicativeDo #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module Distribution.Package.Dhall
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
  , executable
  , testSuite
  , benchmark
  , foreignLib
  , buildType
  , versionRange
  , version
  , configRecordType
  ) where

import Control.Exception ( Exception, throwIO )
import Data.Function ( (&) )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )
import Data.Text.Buildable ( Buildable(..) )
import Text.Trifecta.Delta ( Delta(..) )

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Data.Text as StrictText
import qualified Data.Text.Encoding as StrictText
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Dhall
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



packageIdentifier :: RecordBuilder Cabal.PackageIdentifier
packageIdentifier = do
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
    keyValue "custom-setup" ( Dhall.maybe setupBuildInfo )

  dataFiles <-
    keyValue "data-files" ( Dhall.list Dhall.string )

  dataDir <-
    keyValue "data-dir" Dhall.string

  extraSrcFiles <-
    keyValue "extra-source-files" ( Dhall.list Dhall.string )

  extraTmpFiles <-
    keyValue "extra-tmp-files" ( Dhall.list Dhall.string )

  extraDocFiles <-
    keyValue "extra-doc-files" ( Dhall.list Dhall.string )

  return Cabal.PackageDescription { .. }



version :: Dhall.Type Cabal.Version
version =
  let
    parse builder =
      fromMaybe
        ( error "Could not parse version" )
        ( Cabal.simpleParse ( LazyText.unpack ( Builder.toLazyText builder ) ) )

    extract =
      \case
        LamArr _Version (LamArr _v v) ->
          go v

        e ->
          error ( show e )

    go =
      \case
        Expr.App ( V0 "v" ) ( Expr.TextLit ( Expr.Chunks [] builder ) ) ->
          return ( parse builder )

        e ->
          error ( show e )

    expected =
      Expr.Pi "Version" ( Expr.Const Expr.Type )
        $ Expr.Pi
            "v"
            ( Expr.Pi "_" ( Dhall.expected Dhall.string ) ( V0 "Version" ) )
            ( V0 "Version" )

  in Dhall.Type { .. }



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
    keyValue "includes" ( Dhall.list Dhall.string )

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
    keyValue "build-depends" ( Dhall.list dependency )

  mixins <-
    keyValue "mixins" ( Dhall.list mixin )

  return Cabal.BuildInfo { ..  }



testSuite :: Dhall.Type Cabal.TestSuite
testSuite =
  makeRecord $ do
    testName <-
      pure ""

    testBuildInfo <-
      buildInfo

    testInterface <-
      keyValue "type" testSuiteInterface

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
              <$> makeRecord ( keyValue "main-is" Dhall.string )
          )
        , ( "detailed"
          , Cabal.TestSuiteLibV09 ( Cabal.mkVersion [ 0, 9 ] )
              <$> makeRecord ( keyValue "module" moduleName )
          )
        ]
    )




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
      keyValue "lib-version-info" ( Dhall.maybe versionInfo )

    foreignLibVersionLinux <-
      keyValue "lib-version-linux" ( Dhall.maybe version )

    foreignLibModDefFile <-
      keyValue "mod-def-files" ( Dhall.list Dhall.string )

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
      keyValue "signatures" ( Dhall.list moduleName )

    libExposed <-
      pure True

    pure Cabal.Library { .. }



sourceRepo :: Dhall.Type Cabal.SourceRepo
sourceRepo =
  makeRecord $ do
    repoKind <-
      keyValue "kind" repoKind

    repoType <-
      keyValue "type" ( Dhall.maybe repoType )

    repoLocation <-
      keyValue "location" ( Dhall.maybe Dhall.string )

    repoModule <-
      keyValue "module" ( Dhall.maybe Dhall.string )

    repoBranch <-
      keyValue "branch" ( Dhall.maybe Dhall.string )

    repoTag <-
      keyValue "tag" ( Dhall.maybe Dhall.string )

    repoSubdir <-
      keyValue "subdir" ( Dhall.maybe filePath )

    pure Cabal.SourceRepo { .. }



repoKind :: Dhall.Type Cabal.RepoKind
repoKind =
  Dhall.genericAuto



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



dhallToCabal :: FilePath -> LazyText.Text -> IO Cabal.GenericPackageDescription
dhallToCabal fileName source =
  input fileName source genericPackageDescription


input :: FilePath -> LazyText.Text -> Dhall.Type a -> IO a
input fileName source t = do
  let
    delta =
      Directed ( StrictText.encodeUtf8 ( StrictText.pack fileName ) ) 0 0 0 0

  expr  <-
    throws ( Dhall.Parser.exprFromText delta source )

  expr' <-
    Dhall.Import.load expr

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
    throws (Dhall.TypeCheck.typeOf annot)

  case Dhall.extract t ( Dhall.Core.normalize expr' ) of
    Just x  ->
      return x

    Nothing ->
      throwIO Dhall.InvalidType

  where

    throws :: Exception e => Either e a -> IO a
    throws =
      either throwIO return



pattern LamArr :: Expr.Expr s a -> Expr.Expr s a -> Expr.Expr s a
pattern LamArr a b <- Expr.Lam _ a b



pattern V0 v = Expr.Var ( Expr.V v 0 )



versionRange :: Dhall.Type Cabal.VersionRange
versionRange =
  let
    extract =
      \case
        LamArr _VersionRange (LamArr _anyVersion (LamArr _noVersion (LamArr _thisVersion (LamArr _notThisVersion (LamArr _laterVersion (LamArr _earlierVersion (LamArr _orLaterVersion (LamArr _orEarlierVersion (LamArr _withinVersion (LamArr _majorBoundVersion (LamArr _unionVersionRanges (LamArr _intersectVersionRanges (LamArr _differenceVersionRanges (LamArr _invertVersionRange versionRange)))))))))))))) ->
          go versionRange

        _ ->
          Nothing

    go =
      \case
        V0 "anyVersion" ->
          return Cabal.anyVersion

        V0 "noVersion" ->
          return Cabal.noVersion

        Expr.App ( V0 "thisVersion" ) components ->
          Cabal.thisVersion <$> Dhall.extract version components

        Expr.App ( V0 "notThisVersion" ) components ->
          Cabal.notThisVersion <$> Dhall.extract version components

        Expr.App ( V0 "laterVersion" ) components ->
          Cabal.laterVersion <$> Dhall.extract version components

        Expr.App ( V0 "earlierVersion" ) components ->
          Cabal.earlierVersion <$> Dhall.extract version components

        Expr.App ( V0 "orLaterVersion" ) components ->
          Cabal.orLaterVersion <$> Dhall.extract version components

        Expr.App ( V0 "orEarlierVersion" ) components ->
          Cabal.orEarlierVersion <$> Dhall.extract version components

        Expr.App ( Expr.App ( V0 "unionVersionRanges" ) a ) b ->
          Cabal.unionVersionRanges <$> go a <*> go b

        Expr.App ( Expr.App ( V0 "intersectVersionRanges" ) a ) b ->
          Cabal.intersectVersionRanges <$> go a <*> go b

        Expr.App ( Expr.App ( V0 "differenceVersionRanges" ) a ) b ->
          Cabal.differenceVersionRanges <$> go a <*> go b

        Expr.App ( V0 "invertVersionRange" ) components ->
          Cabal.invertVersionRange <$> go components

        Expr.App ( V0 "withinVersion" ) components ->
          Cabal.withinVersion <$> Dhall.extract version components

        Expr.App ( V0 "majorBoundVersion" ) components ->
          Cabal.majorBoundVersion <$> Dhall.extract version components

        _ ->
          Nothing

    expected =
      let
        versionRange =
          V0 "VersionRange"

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
  Dhall.genericAuto



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
  Dhall.genericAuto



repoType :: Dhall.Type Cabal.RepoType
repoType =
  Dhall.genericAuto



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
  Dhall.genericAuto



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
  let
    knownExtension =
      Dhall.genericAuto

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
          Expr.Union ( Expr.Bool <$ alts )

        _ ->
          error "Could not derive extension type"

  in Dhall.Type { .. }



guarded
  :: ( Monoid a, Eq a, Show a, Diffable a )
  => Dhall.Type a
  -> Dhall.Type ( Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] a )
guarded t =
  let
    extractGuard body =
      case body of
        Expr.BoolLit b ->
          return ( Cabal.Lit b )

        Expr.BoolAnd a b ->
          Cabal.CAnd <$> extractGuard a <*> extractGuard b

        Expr.BoolOr a b ->
          Cabal.COr <$> extractGuard a <*> extractGuard b

        Expr.BoolEQ a b ->
          Cabal.COr
            <$> ( Cabal.CAnd <$> extractGuard a <*> extractGuard b )
            <*> ( Cabal.CAnd
                    <$> ( Cabal.CNot <$> extractGuard a )
                    <*> ( Cabal.CNot <$> extractGuard b )
                )

        -- a != b ==> (a && !y) || (!a && y), but I think the following is
        -- cleaner.
        Expr.BoolNE a b ->
          Cabal.CNot <$> extractGuard ( Expr.BoolEQ a b )

        Expr.BoolIf p a b ->
          Cabal.COr
            <$> ( Cabal.CAnd <$> extractGuard p <*> extractGuard a )
            <*> ( Cabal.CAnd
                    <$> ( Cabal.CNot <$> extractGuard p )
                    <*> extractGuard b
                )

        Expr.App ( Expr.App ( Expr.Field ( V0 "config" ) "impl" ) compiler ) version ->
          Cabal.Var
            <$> ( Cabal.Impl
                    <$> Dhall.extract compilerFlavor compiler
                    <*> Dhall.extract versionRange version
                )

        Expr.App ( Expr.Field ( V0 "config" ) field ) x ->
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

    extract expr =
      configTreeToCondTree <$> extractConfigTree ( toConfigTree expr )

    extractConfigTree ( Leaf a ) =
      Leaf <$> Dhall.extract t a

    extractConfigTree ( Branch cond a b ) =
      Branch <$> extractGuard cond <*> extractConfigTree a <*> extractConfigTree b

    configTreeToCondTree = \case
      Leaf a -> do
        Cabal.CondNode a mempty mempty

      Branch guard a b ->
        let
          true =
            configTreeToCondTree a

          false =
            configTreeToCondTree b

          ( common, true', false' ) =
            diff ( Cabal.condTreeData true ) ( Cabal.condTreeData false )

          ( duplicates, true'', false'' ) =
            diff ( Cabal.condTreeComponents false ) ( Cabal.condTreeComponents true )

        in
          Cabal.CondNode
            common
            mempty
            ( Cabal.CondBranch
                guard
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

    expected =
        Expr.Pi "_" configRecordType ( Dhall.expected t )

  in Dhall.Type { .. }



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



setupBuildInfo :: Dhall.Type Cabal.SetupBuildInfo
setupBuildInfo =
  makeRecord $ do
    setupDepends <-
      keyValue "setup-depends" ( Dhall.list dependency )

    defaultSetupDepends <-
      pure False

    return Cabal.SetupBuildInfo { .. }



filePath :: Dhall.Type FilePath
filePath =
  Dhall.string



mixin :: Dhall.Type Cabal.Mixin
mixin =
  makeRecord $ do
    mixinPackageName <-
      keyValue "package" packageName

    mixinIncludeRenaming <-
      keyValue "renaming" includeRenaming

    pure Cabal.Mixin { .. }



includeRenaming :: Dhall.Type Cabal.IncludeRenaming
includeRenaming =
  makeRecord $ do
    includeProvidesRn <-
      keyValue "provides" moduleRenaming

    includeRequiresRn <-
      keyValue "requires" moduleRenaming

    pure Cabal.IncludeRenaming { .. }



moduleRenaming :: Dhall.Type Cabal.ModuleRenaming
moduleRenaming =
  fmap Cabal.ModuleRenaming $
  Dhall.list $
  makeRecord $
    (,) <$> keyValue "rename" moduleName <*> keyValue "to" moduleName
