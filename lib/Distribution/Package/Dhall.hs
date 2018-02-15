{-# language ApplicativeDo #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module Distribution.Package.Dhall
  ( dhallToCabal
  , dhallToCabalContext
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
  , guard
  ) where

import Control.Exception ( Exception, throwIO )
import Data.Function ( (&) )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )
import Data.Text.Buildable ( Buildable(..) )
import Text.Trifecta.Delta ( Delta(..) )

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text as StrictText
import qualified Data.Text.Encoding as StrictText
import qualified Data.Vector as Vector
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
    Dhall.Import.loadWithContext dhallToCabalContext expr

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
    throws (Dhall.TypeCheck.typeWith dhallToCabalContext annot)

  case Dhall.extract t ( Dhall.Core.normalizeWith normalizer expr' ) of
    Just x  ->
      return x

    Nothing ->
      throwIO Dhall.InvalidType

  where

    throws :: Exception e => Either e a -> IO a
    throws =
      either throwIO return



-- | The custom context used for type-checking @dhall-to-cabal@ Dhall
-- expressions.
--
-- This context provides the following:
--
-- * @VersionRange : Type@
-- * @anyVersion : VersionRange@
-- * @noVersion : VersionRange@
-- * @thisVersion : List Natural -> VersionRange@
-- * @notThisVersion : List Natural -> VersionRange@
-- * @laterVersion : List Natural -> VersionRange@
-- * @earlierVersion : List Natural -> VersionRange@
-- * @orLaterVersion : List Natural -> VersionRange@
-- * @orEarlierVersion : List Natural -> VersionRange@
-- * @withinVersion : List Natural -> VersionRange@
-- * @majorBoundVersion : List Natural -> VersionRange@
-- * @unionVersionRanges : VersionRange -> VersionRange -> VersionRange@
-- * @intersectVersionRanges : VersionRange -> VersionRange -> VersionRange@
-- * @differenceVersionRanges : VersionRange -> VersionRange -> VersionRange@
-- * @invertVersionRange : VersionRange -> VersionRange@
-- * @v : Text -> List Natural@

dhallToCabalContext
  :: Ctx.Context ( Expr.Expr Dhall.Parser.Src Dhall.TypeCheck.X )
dhallToCabalContext =
  let
    versionRangeType =
      Dhall.expected versionRange

    versionType =
      Dhall.expected version

    fun a b =
      Expr.Pi "_" a b

  in
  Ctx.empty
    & Ctx.insert "VersionRange" ( Expr.Const Expr.Type )
    & Ctx.insert "anyVersion" versionRangeType
    & Ctx.insert "noVersion" versionRangeType
    & Ctx.insert "thisVersion" ( fun versionType versionRangeType )
    & Ctx.insert "notThisVersion" ( fun versionType versionRangeType )
    & Ctx.insert "laterVersion" ( fun versionType versionRangeType )
    & Ctx.insert "earlierVersion" ( fun versionType versionRangeType )
    & Ctx.insert "orLaterVersion" ( fun versionType versionRangeType )
    & Ctx.insert "orEarlierVersion" ( fun versionType versionRangeType )
    & Ctx.insert "withinVersion" ( fun versionType versionRangeType)
    & Ctx.insert "majorBoundVersion" ( fun versionType versionRangeType)
    & Ctx.insert
        "unionVersionRanges"
        ( fun versionRangeType ( fun versionRangeType versionRangeType ) )
    & Ctx.insert
        "intersectVersionRanges"
        ( fun versionRangeType ( fun versionRangeType versionRangeType ) )
    & Ctx.insert
        "differenceVersionRanges"
        ( fun versionRangeType ( fun versionRangeType versionRangeType ) )
    & Ctx.insert
        "invertVersionRange"
        ( fun versionRangeType ( fun versionRangeType versionRangeType ) )
    & Ctx.insert "v" ( fun Expr.Text versionType )



normalizer :: Dhall.Core.Normalizer a
normalizer = \case
  Expr.App ( Expr.Var ( Expr.V "v" 0 ) ) ( Expr.TextLit ( Expr.Chunks [] builder ) ) ->
    Just ( toDhall parse )

    where

      parse =
        fromMaybe
          ( error "Could not parse version" )
          ( Cabal.simpleParse ( LazyText.unpack ( Builder.toLazyText builder ) ) )

      toDhall v =
        Expr.ListLit
          ( Just ( Expr.App Expr.List Expr.Natural ) ) -- Dhall.expected version
          ( Vector.fromList
              ( Expr.NaturalLit . fromIntegral <$> Cabal.versionNumbers v )
          )

  _ ->
    Nothing



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
