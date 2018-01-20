{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}

module Distribution.Package.Dhall where

import Control.Exception ( Exception, throwIO )
import Control.Monad ( (>=>), guard )
import Data.Foldable ( toList )
import Data.Function ( (&) )
import Data.Monoid
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
import qualified Distribution.Types.ExecutableScope as Cabal
import qualified Distribution.Types.ForeignLib as Cabal
import qualified Distribution.Types.ForeignLibType as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Version as Cabal

import qualified Dhall.Core as Expr
  ( Const(..), Expr(..), Normalizer, Var(..) )


packageIdentifier :: Dhall.Type Cabal.PackageIdentifier
packageIdentifier =
  let
    extract expr = do
      Expr.RecordLit fields <-
        return expr

      pkgName <-
        Map.lookup "name" fields >>= Dhall.extract packageName

      pkgVersion <-
        Map.lookup "version" fields >>= Dhall.extract version

      return Cabal.PackageIdentifier { .. }

    expected =
      Expr.Record
        ( Map.fromList
            [ ( "name", Expr.Text )
            , ( "version", Dhall.expected version )
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
      Expr.RecordLit fields <-
        return expr

      field <-
        return ( flip Map.lookup fields )

      package <-
        field "package" >>= Dhall.extract packageIdentifier

      benchmarks <-
        field "benchmarks" >>= Dhall.extract ( list benchmark )

      testSuites <-
        field "tests" >>= Dhall.extract ( list testSuite )

      executables <-
        field "executables" >>= Dhall.extract ( list executable )

      foreignLibs <-
        field "foreign-libraries" >>= Dhall.extract ( list foreignLib )

      subLibraries <-
        field "sub-libraries" >>= Dhall.extract ( list library )

      library <-
        field "library" >>= Dhall.extract ( Dhall.maybe library )

      customFieldsPD <-
        field "x-fields"
          >>= Dhall.extract ( list ( pair string string ) )

      sourceRepos <-
        field "source-repos" >>= Dhall.extract ( list sourceRepo )

      specVersionRaw <-
        Left <$> ( field "cabal-version" >>= Dhall.extract version )

      buildType <-
        field "build-type" >>= Dhall.extract ( Dhall.maybe buildType )

      license <-
        field "license" >>= Dhall.extract license

      licenseFiles <-
        field "license-files" >>= Dhall.extract ( list string )

      copyright <-
        field "copyright" >>= Dhall.extract string

      maintainer <-
        field "maintainer" >>= Dhall.extract string

      author <-
        field "author" >>= Dhall.extract string

      stability <-
        field "stability" >>= Dhall.extract string

      testedWith <-
        field "tested-with" >>= Dhall.extract ( list compiler )

      homepage <-
        field "homepage" >>= Dhall.extract string

      pkgUrl <-
        return ""

      bugReports <-
        field "bug-reports" >>= Dhall.extract string

      synopsis <-
        field "synopsis" >>= Dhall.extract string

      description <-
        field "description" >>= Dhall.extract string

      category <-
        field "category" >>= Dhall.extract string

      -- Cabal documentation states
      --
      --   > YOU PROBABLY DON'T WANT TO USE THIS FIELD.
      --
      -- So I guess we won't use this field.
      buildDepends <-
        return []

      setupBuildInfo <-
        return Nothing

      dataFiles <-
        field "data-files" >>= Dhall.extract ( list string )

      dataDir <-
        field "data-directory" >>= Dhall.extract string

      extraSrcFiles <-
        field "extra-source-files" >>= Dhall.extract ( list string )

      extraTmpFiles <-
        field "extra-temp-files" >>= Dhall.extract ( list string )

      extraDocFiles <-
        field "extra-doc-files" >>= Dhall.extract ( list string )

      return Cabal.PackageDescription { .. }

    fieldTypes =
      [ ( "package", Dhall.expected packageIdentifier )
      , ( "benchmarks", Dhall.expected ( list benchmark ) )
      , ( "tests", Dhall.expected ( list testSuite ) )
      , ( "executables", Dhall.expected ( list executable ) )
      , ( "foreign-libraries", Dhall.expected ( list foreignLib ) )
      , ( "library", Dhall.expected ( Dhall.maybe library ) )
      , ( "sub-libraries", Dhall.expected ( list library ) )
      , ( "x-fields" , Dhall.expected ( list ( pair string string ) ) )
      , ( "source-repos", Dhall.expected ( list sourceRepo ) )
      , ( "cabal-version", Dhall.expected version )
      , ( "build-type", Dhall.expected ( Dhall.maybe buildType ) )
      , ( "license", Dhall.expected license )
      , ( "license-files", Dhall.expected ( list string ) )
      , ( "copyright", Dhall.expected string )
      , ( "maintainer", Dhall.expected string )
      , ( "author", Dhall.expected string )
      , ( "stability", Dhall.expected string )
      , ( "tested-with", Dhall.expected ( list compiler ) )
      , ( "homepage", Dhall.expected string )
      , ( "bug-reports", Dhall.expected string )
      , ( "synopsis", Dhall.expected string )
      , ( "description", Dhall.expected string )
      , ( "category", Dhall.expected string )
      , ( "data-files", Dhall.expected ( list string ) )
      , ( "data-directory", Dhall.expected string )
      , ( "extra-source-files", Dhall.expected ( list string ) )
      , ( "extra-temp-files", Dhall.expected ( list string ) )
      , ( "extra-doc-files", Dhall.expected ( list string ) )
      ]

    expected =
      Expr.Record ( Map.fromList fieldTypes )

  in Dhall.Type { .. }



version :: Dhall.Type Cabal.Version
version =
  Cabal.mkVersion <$> list ( fromIntegral <$> Dhall.natural )



benchmark :: Dhall.Type Cabal.Benchmark
benchmark =
  let
    extract expr = do
      Expr.RecordLit fields <-
        return expr

      mainIs <-
        Map.lookup "main-is" fields >>= Dhall.extract string

      benchmarkName <-
        Map.lookup "name" fields
          >>= Dhall.extract unqualComponentName

      benchmarkInterface <-
        return ( Cabal.BenchmarkExeV10 ( Cabal.mkVersion [ 1, 0 ] ) mainIs )

      benchmarkBuildInfo <-
        Dhall.extract buildInfo expr

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
      Expr.RecordLit fields <-
        return expr

      buildable <-
        return True

      buildTools <-
        return []

      buildToolDepends <-
        return []

      cppOptions <-
        return []

      ccOptions <-
        return []

      ldOptions <-
        return []

      pkgconfigDepends <-
        return []

      frameworks <-
        return []

      extraFrameworkDirs <-
        return []

      cSources <-
        return []

      jsSources <-
        return []

      hsSourceDirs <-
        Map.lookup "hs-source-dirs" fields >>= Dhall.extract ( list string )

      otherModules <-
        Map.lookup "other-modules" fields >>= Dhall.extract ( list moduleName )

      autogenModules <-
        return []

      defaultLanguage <-
        return Nothing

      otherLanguages <-
        return []

      defaultExtensions <-
        return []

      otherExtensions <-
        return []

      oldExtensions <-
        return []

      extraLibs <-
        return []

      extraGHCiLibs <-
        return []

      extraLibDirs <-
        return []

      includeDirs <-
        return []

      includes <-
        return []

      installIncludes <-
        return []

      options <-
        return []

      profOptions <-
        return []

      sharedOptions <-
        return []

      customFieldsBI <-
        return []

      targetBuildDepends <-
        Map.lookup "build-dependencies" fields
          >>= Dhall.extract ( list dependency )

      mixins <-
        return []

      return Cabal.BuildInfo { ..  }

    expected =
      Expr.Record buildInfoFields

  in Dhall.Type { .. }



buildInfoFields =
  Map.fromList
    [ ( "build-dependencies" , Dhall.expected ( list dependency ) )
    , ( "other-modules", Dhall.expected ( list moduleName ) )
    , ( "hs-source-dirs", Dhall.expected ( list string ) )
    ]



testSuite :: Dhall.Type Cabal.TestSuite
testSuite =
  let
    extract expr = do
      Expr.RecordLit fields <-
        return expr

      testName <-
        Map.lookup "name" fields >>= Dhall.extract unqualComponentName

      mainIs <-
        Map.lookup "main-is" fields >>= Dhall.extract string

      testInterface <-
        return ( Cabal.TestSuiteExeV10 ( Cabal.mkVersion [ 1, 0 ] ) mainIs )

      testBuildInfo <-
        Dhall.extract buildInfo expr

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
      Expr.RecordLit fields <-
        return expr

      exeName <-
        Map.lookup "name" fields >>= Dhall.extract unqualComponentName

      modulePath <-
        Map.lookup "main-is" fields >>= Dhall.extract string

      exeScope <-
        return Cabal.ExecutablePublic

      buildInfo <-
        Dhall.extract buildInfo expr

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
      Expr.RecordLit fields <-
        return expr

      foreignLibName <-
        Map.lookup "name" fields >>= Dhall.extract unqualComponentName

      foreignLibType <-
        Map.lookup "type" fields >>= Dhall.extract foreignLibType

      foreignLibOptions <-
        return []

      foreignLibBuildInfo <-
        Dhall.extract buildInfo expr

      foreignLibVersionInfo <-
        return Nothing

      foreignLibVersionLinux <-
        return Nothing

      foreignLibModDefFile <-
        return []

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
      Expr.UnionLit t ( Expr.RecordLit fields ) _ <-
        return expr

      guard (Map.null fields)

      case t of
        "Shared" ->
          return Cabal.ForeignLibNativeShared

        _ ->
          Nothing


    expected =
      Expr.Union ( Map.fromList [ ( "Shared", Expr.Record Map.empty ) ] )

  in Dhall.Type { .. }



library :: Dhall.Type Cabal.Library
library =
  let
    extract expr = do
      Expr.RecordLit fields <-
        return expr

      libName <-
        Map.lookup "name" fields
          >>= Dhall.extract ( Dhall.maybe unqualComponentName )

      libBuildInfo <-
        Dhall.extract buildInfo expr

      exposedModules <-
        Map.lookup "exposed-modules" fields
          >>= Dhall.extract ( list moduleName )

      reexportedModules <-
        return []

      signatures <-
        return []

      libExposed <-
        return True

      return Cabal.Library { .. }

    expected =
      Expr.Record
        ( Map.union
            ( Map.fromList
                [ ( "name", Dhall.expected ( Dhall.maybe unqualComponentName ) )
                , ( "exposed-modules" , Dhall.expected ( list moduleName )
                  )
                ] )
            buildInfoFields
        )

  in Dhall.Type { .. }



sourceRepo :: Dhall.Type Cabal.SourceRepo
sourceRepo =
  let
    extract expr = do
      Expr.RecordLit fields <-
        return expr

      repoKind <-
        return Cabal.RepoHead

      repoType <-
        return Nothing

      repoLocation <-
        return Nothing

      repoModule <-
        return Nothing

      repoBranch <-
        return Nothing

      repoTag <-
        return Nothing

      repoSubdir <-
        return Nothing

      return Cabal.SourceRepo { .. }

    expected =
      Expr.Record Map.empty

  in Dhall.Type { .. }



dependency :: Dhall.Type Cabal.Dependency
dependency =
  let
    extract expr = do
      Expr.RecordLit fields <-
        return expr

      packageName <-
        Map.lookup "package" fields
          >>= Dhall.extract packageName

      versionRange <-
        Map.lookup "bounds" fields
          >>= Dhall.extract versionRange

      return ( Cabal.Dependency packageName versionRange )

    expected =
      Expr.Record
        ( Map.fromList
            [ ( "package", Dhall.expected packageName )
            , ( "bounds", Expr.Var ( Expr.V "VersionRange" 0 ) )
            ] )

  in Dhall.Type { .. }



moduleName :: Dhall.Type Cabal.ModuleName
moduleName =
  let
    extract =
      exprToString >=> Cabal.simpleParse

    expected =
      Expr.Text

  in Dhall.Type { .. }



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
  let
    extract expr = do
      Expr.UnionLit ctor ( Expr.RecordLit fields ) _ <-
        return expr

      guard (Map.null fields)

      case ctor of
        "Simple" ->
          return Cabal.Simple

        "Configure" ->
          return Cabal.Configure

        "Make" ->
          return Cabal.Make

        "Custom" ->
          return Cabal.Custom

        _ ->
          Nothing

    expected =
      Expr.Union
        ( Map.fromList
            [ ("Simple", Expr.Record Map.empty)
            , ("Configure", Expr.Record Map.empty)
            , ("Make", Expr.Record Map.empty)
            , ("Custom", Expr.Record Map.empty)
            ]
        )

  in Dhall.Type { .. }



license :: Dhall.Type Cabal.License
license =
  let
    extract expr = do
      Expr.UnionLit ctor ctorFields _ <-
        return expr

      case ctor of
        "GPL" -> do
          gplVersion <-
            Dhall.extract ( Dhall.maybe version ) ctorFields

          return ( Cabal.GPL gplVersion )

        _ ->
          Nothing

    expected =
      Expr.Union
        ( Map.fromList
            [ ("GPL", Dhall.expected ( Dhall.maybe version ) )
            ]
        )

  in Dhall.Type { .. }



pair :: Dhall.Type a -> Dhall.Type b -> Dhall.Type ( a, b )
pair l r =
  let
    extract expr = do
      Expr.RecordLit elems <-
        return expr

      (,) <$> ( Map.lookup "_1" elems >>= Dhall.extract l )
          <*> ( Map.lookup "_2" elems >>= Dhall.extract r )

    expected =
      Expr.Record
        ( Map.fromList
            [ ( "_1", Dhall.expected l )
            , ( "_2", Dhall.expected r )
            ]
        )

  in Dhall.Type { .. }



compiler :: Dhall.Type ( Cabal.CompilerFlavor, Cabal.VersionRange )
compiler =
  let
    extract expr = do
      Expr.RecordLit fields <-
        return expr

      (,)
        <$> ( Map.lookup "compiler" fields >>= Dhall.extract compilerFlavor )
        <*> ( Map.lookup "version" fields >>= Dhall.extract versionRange )

    expected =
      Expr.Record
        ( Map.fromList
            [ ( "compiler", Dhall.expected compilerFlavor )
            , ( "version", Dhall.expected versionRange )
            ]
        )

  in Dhall.Type { .. }



compilerFlavor :: Dhall.Type Cabal.CompilerFlavor
compilerFlavor =
  let
    extract expr = do
      Expr.UnionLit ctor v _ <-
        return expr

      case ctor of
        "GHC" ->
          return Cabal.GHC

        _ ->
          Nothing

    expected =
      Expr.Union
        ( Map.fromList
            [ ( "GHC", Expr.Record Map.empty )
            ]
        )

  in Dhall.Type { .. }



list :: Dhall.Type a -> Dhall.Type [a]
list t =
  toList <$> Dhall.vector t
