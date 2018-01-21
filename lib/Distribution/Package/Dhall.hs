{-# language ApplicativeDo #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module Distribution.Package.Dhall where

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
import qualified Dhall.Core as Dhall ( Expr )
import qualified Dhall.Import
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Distribution.Compiler as Cabal
import qualified Distribution.License as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Text as Cabal ( simpleParse )
import qualified Distribution.Types.Dependency as Cabal
import qualified Distribution.Types.ExeDependency as Cabal
import qualified Distribution.Types.ExecutableScope as Cabal
import qualified Distribution.Types.ForeignLib as Cabal
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
  Cabal.mkPackageName <$> string



packageDescription :: Dhall.Type Cabal.PackageDescription
packageDescription =
  makeRecord $ do
    package <-
      keyValue "package" packageIdentifier

    benchmarks <-
      keyValue "benchmarks" ( list benchmark )

    testSuites <-
      keyValue "tests" ( list testSuite )

    executables <-
      keyValue "executables" ( list executable )

    foreignLibs <-
      keyValue "foreign-libraries" ( list foreignLib )

    subLibraries <-
      keyValue "sub-libraries" ( list library )

    library <-
      keyValue "library" ( Dhall.maybe library )

    customFieldsPD <-
      keyValue "x-fields" ( list ( pair string string ) )

    sourceRepos <-
      keyValue "source-repos" ( list sourceRepo )

    specVersionRaw <-
      Left <$> ( keyValue "cabal-version" version )

    buildType <-
      keyValue "build-type" ( Dhall.maybe buildType )

    license <-
      keyValue "license" license

    licenseFiles <-
      keyValue "license-files" ( list string )

    copyright <-
      keyValue "copyright" string

    maintainer <-
      keyValue "maintainer" string

    author <-
      keyValue "author" string

    stability <-
      keyValue "stability" string

    testedWith <-
      keyValue "tested-with" ( list compiler )

    homepage <-
      keyValue "homepage" string

    pkgUrl <-
      keyValue "package-url" string

    bugReports <-
      keyValue "bug-reports" string

    synopsis <-
      keyValue "synopsis" string

    description <-
      keyValue "description" string

    category <-
      keyValue "category" string

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
      keyValue "data-files" ( list string )

    dataDir <-
      keyValue "data-directory" string

    extraSrcFiles <-
      keyValue "extra-source-files" ( list string )

    extraTmpFiles <-
      keyValue "extra-temp-files" ( list string )

    extraDocFiles <-
      keyValue "extra-doc-files" ( list string )

    return Cabal.PackageDescription { .. }



version :: Dhall.Type Cabal.Version
version =
  Cabal.mkVersion <$> list ( fromIntegral <$> Dhall.natural )



benchmark :: Dhall.Type Cabal.Benchmark
benchmark =
  makeRecord $ do
    mainIs <-
      keyValue "main-is" string

    benchmarkName <-
      keyValue "name" unqualComponentName

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
    keyValue "build-tools" ( list legacyExeDependency )

  buildToolDepends <-
    keyValue "build-tool-depends" ( list exeDependency )

  cppOptions <-
    keyValue "cpp-options" ( list string )

  ccOptions <-
    keyValue "cc-options" ( list string )

  ldOptions <-
    keyValue "ld-options" ( list string )

  pkgconfigDepends <-
    keyValue "pkgconfig-depends" ( list pkgconfigDependency )

  frameworks <-
    keyValue "frameworks" ( list string )

  extraFrameworkDirs <-
    keyValue "extra-framework-dirs" ( list string )

  cSources <-
    keyValue "c-sources" ( list string )

  jsSources <-
    keyValue "js-sources" ( list string )

  hsSourceDirs <-
    keyValue "hs-source-dirs" ( list string )

  otherModules <-
    keyValue "other-modules" ( list moduleName )

  autogenModules <-
    keyValue "autogen-modules" ( list moduleName )

  defaultLanguage <-
    keyValue "default-language" ( Dhall.maybe language )

  otherLanguages <-
    keyValue "other-languages" ( list language )

  defaultExtensions <-
    keyValue "default-extensions" ( list extension )

  otherExtensions <-
    keyValue "other-extensions" ( list extension )

  oldExtensions <-
    pure []

  extraLibs <-
    keyValue "extra-libraries" ( list string )

  extraGHCiLibs <-
    keyValue "extra-ghci-libraries" ( list string )

  extraLibDirs <-
    keyValue "extra-lib-dirs" ( list string )

  includeDirs <-
    keyValue "include-dirs" ( list string )

  includes <-
    keyValue "include" ( list string )

  installIncludes <-
    keyValue "install-includes" ( list string )

  options <-
    keyValue "compiler-options" compilerOptions

  profOptions <-
    keyValue "profiling-options" compilerOptions

  sharedOptions <-
    keyValue "shared-options" compilerOptions

  customFieldsBI <-
    pure []

  targetBuildDepends <-
    keyValue "build-dependencies" ( list dependency )

  mixins <-
    pure []

  return Cabal.BuildInfo { ..  }



testSuite :: Dhall.Type Cabal.TestSuite
testSuite =
  makeRecord $ do
    testName <-
      keyValue "name" unqualComponentName

    mainIs <-
      keyValue "main-is" string

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
  Cabal.mkUnqualComponentName <$> string



executable :: Dhall.Type Cabal.Executable
executable =
  makeRecord $ do
    exeName <-
      keyValue "name" unqualComponentName

    modulePath <-
      keyValue "main-is" string

    exeScope <-
      keyValue "scope" executableScope

    buildInfo <-
      buildInfo

    pure Cabal.Executable { .. }



foreignLib :: Dhall.Type Cabal.ForeignLib
foreignLib =
  makeRecord $ do
    foreignLibName <-
      keyValue "name" unqualComponentName

    foreignLibType <-
      keyValue "type" foreignLibType

    foreignLibOptions <-
      pure []

    foreignLibBuildInfo <-
      buildInfo

    foreignLibVersionInfo <-
      pure Nothing

    foreignLibVersionLinux <-
      pure Nothing

    foreignLibModDefFile <-
      pure []

    pure Cabal.ForeignLib { .. }



foreignLibType :: Dhall.Type Cabal.ForeignLibType
foreignLibType =
  makeUnion
    ( Map.fromList
        [ ( "Shared", Cabal.ForeignLibNativeShared <$ emptyRecord )
        ]
    )



library :: Dhall.Type Cabal.Library
library =
  makeRecord $ do
    libName <-
      keyValue "name" ( Dhall.maybe unqualComponentName )

    libBuildInfo <-
      buildInfo

    exposedModules <-
      keyValue "exposed-modules" ( list moduleName )

    reexportedModules <-
      pure []

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
      keyValue "location" ( Dhall.maybe string )

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
    Cabal.simpleParse <$> string



exprToString :: Dhall.Expr a b -> Maybe String
exprToString expr = do
  Expr.TextLit builder <-
    return expr

  return
    ( LazyText.unpack ( Builder.toLazyText builder ) )



dhallFileToCabal :: FilePath -> IO Cabal.PackageDescription
dhallFileToCabal file = do
  source <-
    LazyText.readFile file

  Dhall.detailed ( input source packageDescription )



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
    & Ctx.insert
        "majorVersion"
        ( Expr.Pi
            "_"
            ( Dhall.expected ( list Dhall.natural ) )
            ( Dhall.expected versionRange )
        )
    & Ctx.insert "VersionRange" ( Expr.Const Expr.Type )
    & Ctx.insert "anyVersion" ( Dhall.expected versionRange )



versionRange :: Dhall.Type Cabal.VersionRange
versionRange =
  let
    extract expr =
      case expr of
        Expr.App ( Expr.Var ( Expr.V "majorVersion" 0 ) ) components ->
          Cabal.majorBoundVersion <$> Dhall.extract version components

        Expr.Var ( Expr.V "anyVersion" 0 ) ->
          return Cabal.anyVersion

        _ ->
          Nothing

    expected =
      Expr.Var ( Expr.V "VersionRange" 0 )

  in Dhall.Type { .. }



buildType :: Dhall.Type Cabal.BuildType
buildType =
  makeUnion
    ( Map.fromList
        [ ( "Simple", Cabal.Simple <$ emptyRecord )
        , ( "Configure", Cabal.Configure <$ emptyRecord )
        , ( "Make", Cabal.Make <$ emptyRecord )
        , ( "Custom", Cabal.Custom <$ emptyRecord )
        ]
    )



license :: Dhall.Type Cabal.License
license =
  makeUnion
    ( Map.fromList
        [ ( "GPL", Cabal.GPL <$> Dhall.maybe version )
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
        [ ( "GHC", Cabal.GHC <$ emptyRecord ) ]
    )



repoType :: Dhall.Type Cabal.RepoType
repoType =
  makeUnion
    ( Map.fromList
        [ ( "Git", Cabal.Git <$ emptyRecord ) ]
    )



legacyExeDependency :: Dhall.Type Cabal.LegacyExeDependency
legacyExeDependency =
  makeRecord $ do
    exe <-
      keyValue "exe" string

    version <-
      keyValue "version" versionRange

    pure ( Cabal.LegacyExeDependency exe version )



compilerOptions :: Dhall.Type [ ( Cabal.CompilerFlavor, [ String ] ) ]
compilerOptions =
  makeRecord $
    sequenceA
      [ (,) <$> pure Cabal.GHC <*> keyValue "GHC" optionsRecord
      ]

  where

    optionsRecord =
      makeRecord $
        keyValue "build-options" ( list string )



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
        [ ( "Haskell98", Cabal.Haskell98 <$ emptyRecord  )
        , ( "Haskell2010", Cabal.Haskell2010 <$ emptyRecord )
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
  Cabal.mkPkgconfigName <$> string



extension :: Dhall.Type Cabal.Extension
extension =
  makeUnion
    ( Map.fromList
        [ ]
    )


executableScope :: Dhall.Type Cabal.ExecutableScope
executableScope = 
  makeUnion
    ( Map.fromList
        [ ( "Public", Cabal.ExecutablePublic <$ emptyRecord )
        , ( "Private", Cabal.ExecutablePrivate <$ emptyRecord )
        ]
    )
