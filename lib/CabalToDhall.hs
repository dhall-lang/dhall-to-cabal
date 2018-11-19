{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

module CabalToDhall
  ( cabalToDhall
  , parseGenericPackageDescriptionThrows
  , KnownDefault (..)
  , PreludeReference (..)
  , resolvePreludeVar
  , getDefault
  ) where

import Data.Foldable ( foldMap )
import Data.Functor.Contravariant ( (>$<), Contravariant( contramap ) )
import Data.Monoid ( First(..) )
import Data.Semigroup ( Semigroup, (<>) )
import GHC.Stack
import Numeric.Natural ( Natural )

import qualified Data.ByteString as ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import qualified Data.Text as StrictText
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Core as Expr ( Expr(..), Var(..), Binding(..) )
import qualified Dhall.Map as Map
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Distribution.Compiler as Cabal
import qualified Distribution.License as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.SPDX as SPDX
import qualified Distribution.System as Cabal
import qualified Distribution.Text as Cabal
import qualified Distribution.Types.Benchmark as Cabal
import qualified Distribution.Types.BenchmarkInterface as Cabal
import qualified Distribution.Types.BuildInfo as Cabal
import qualified Distribution.Types.BuildType as Cabal
import qualified Distribution.Types.CondTree as Cabal
import qualified Distribution.Types.Condition as Cabal
import qualified Distribution.Types.Dependency as Cabal
import qualified Distribution.Types.ExeDependency as Cabal
import qualified Distribution.Types.Executable as Cabal
import qualified Distribution.Types.ExecutableScope as Cabal
import qualified Distribution.Types.ForeignLib as Cabal
import qualified Distribution.Types.ForeignLibOption as Cabal
import qualified Distribution.Types.ForeignLibType as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal
import qualified Distribution.Types.IncludeRenaming as Cabal
import qualified Distribution.Types.LegacyExeDependency as Cabal
import qualified Distribution.Types.Library as Cabal
import qualified Distribution.Types.Mixin as Cabal
import qualified Distribution.Types.ModuleReexport as Cabal
import qualified Distribution.Types.ModuleRenaming as Cabal
import qualified Distribution.Types.PackageDescription as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PkgconfigDependency as Cabal
import qualified Distribution.Types.PkgconfigName as Cabal
import qualified Distribution.Types.SetupBuildInfo as Cabal
import qualified Distribution.Types.SourceRepo as Cabal
import qualified Distribution.Types.TestSuite as Cabal
import qualified Distribution.Types.TestSuiteInterface as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Version as Cabal
import qualified Language.Haskell.Extension as Cabal

import DhallLocation ( DhallLocation(..) )
import DhallToCabal ( sortExpr )
import DhallToCabal.ConfigTree ( ConfigTree(..) )


type DhallExpr =
  Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X


dhallString :: String -> Expr.Expr s a
dhallString = Expr.TextLit . Dhall.Core.Chunks [] . StrictText.pack


parseGenericPackageDescriptionThrows
  :: ByteString.ByteString
  -> IO Cabal.GenericPackageDescription
parseGenericPackageDescriptionThrows source =
  case Cabal.runParseResult ( Cabal.parseGenericPackageDescription source ) of
    (_warnings, Left e) -> do
      putStrLn "Could not parse Cabal file: "

      error ( show e )

    (_warnings, Right genericPackageDescription) ->
      return genericPackageDescription


cabalToDhall
  :: DhallLocation
  -> Cabal.GenericPackageDescription
  -> Expr.Expr Dhall.Parser.Src Dhall.Core.Import
cabalToDhall dhallLocation genericPackageDescription = 
  Expr.Let (NonEmpty.fromList
             [ Expr.Binding "prelude"
               Nothing ( Expr.Embed ( preludeLocation dhallLocation ) )
             , Expr.Binding  "types"
               Nothing ( Expr.Embed ( typesLocation dhallLocation ) )
             ]
           ) $ Dhall.TypeCheck.absurd <$>
                 Dhall.embed
                 genericPackageDescriptionToDhall
                 genericPackageDescription


-- Note: the Show instance is used by --print-type.
data KnownDefault
  = CompilerOptions
  | BuildInfo
  | Library
  | Executable
  | Benchmark
  | TestSuite
  | Package
  | SourceRepo
  deriving ( Bounded, Enum, Eq, Ord, Read, Show )


data PreludeReference
  = PreludeDefault KnownDefault
  | PreludeConstructorsLicense
  | PreludeConstructorsRepoKind
  | PreludeConstructorsScope
  | PreludeV


resolvePreludeVar :: PreludeReference -> Expr.Expr s a
resolvePreludeVar = \case
  PreludeDefault typ ->
    Expr.Var "prelude" `Expr.Field` "defaults" `Expr.Field` StrictText.pack ( show typ )
  PreludeV ->
    Expr.Var "prelude" `Expr.Field` "v"
  PreludeConstructorsLicense ->
    Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "Licenses"
  PreludeConstructorsRepoKind ->
    Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "RepoKind"
  PreludeConstructorsScope ->
    Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "Scopes"


type Default s a
   = ( PreludeReference -> Expr.Expr s a )
   -> Map.Map StrictText.Text ( Expr.Expr s a )


getDefault
  :: ( Eq s )
  => Dhall.Core.Import
  -> ( PreludeReference -> Expr.Expr s Dhall.Core.Import )
  -> KnownDefault
  -> Expr.Expr s Dhall.Core.Import
getDefault typesLoc resolve typ = withTypesImport expr
  where
    withTypesImport =
      Expr.Let $ pure $ Expr.Binding "types" Nothing ( Expr.Embed typesLoc )

    factorBuildInfo fields =
      let
        shared = Map.filter id ( Map.intersectionWith (==) fields ( buildInfoDefault resolve ) )
      in
        if | null shared
             -> Expr.RecordLit fields
           | null ( Map.difference fields shared )
             -> resolve ( PreludeDefault BuildInfo )
           | otherwise
             -> Expr.Prefer
                  ( resolve ( PreludeDefault BuildInfo ) )
                  ( Expr.RecordLit ( Map.difference fields shared ) )

    expr =
      case typ of
        CompilerOptions ->
          Expr.RecordLit ( compilerOptionsDefault resolve )
        BuildInfo ->
          Expr.RecordLit ( buildInfoDefault resolve )
        Library ->
          factorBuildInfo ( libraryDefault resolve )
        Executable ->
          factorBuildInfo ( executableDefault resolve )
        Benchmark ->
          factorBuildInfo ( benchmarkDefault resolve )
        TestSuite ->
          factorBuildInfo ( testSuiteDefault resolve )
        Package ->
          Expr.RecordLit ( packageDefault resolve )
        SourceRepo ->
          Expr.RecordLit ( sourceRepoDefault resolve )


emptyListDefault
  :: StrictText.Text
  -> Expr.Expr s a
  -> ( StrictText.Text, Expr.Expr s a )
emptyListDefault name ty =
  ( name, Expr.ListLit ( Just ty ) mempty )


emptyOptionalDefault
  :: StrictText.Text
  -> Expr.Expr s a
  -> ( StrictText.Text, Expr.Expr s a )
emptyOptionalDefault name ty =
  ( name, Expr.App Expr.None ty )


textFieldDefault
  :: StrictText.Text
  -> StrictText.Text
  -> ( StrictText.Text, Expr.Expr s a )
textFieldDefault name def =
  ( name
  , Expr.TextLit ( Dhall.Core.Chunks [] def )
  )


generaliseDeclared =
  Dhall.Core.denote . fmap Dhall.TypeCheck.absurd . Dhall.declared


compilerOptionsDefault :: Default s a
compilerOptionsDefault _resolve =
  ( Map.fromList
    [ emptyListDefault "Eta" Expr.Text
    , emptyListDefault "GHC" Expr.Text
    , emptyListDefault "GHCJS" Expr.Text
    , emptyListDefault "HBC" Expr.Text
    , emptyListDefault "Helium" Expr.Text
    , emptyListDefault "Hugs" Expr.Text
    , emptyListDefault "JHC" Expr.Text
    , emptyListDefault "LHC" Expr.Text
    , emptyListDefault "NHC" Expr.Text
    , emptyListDefault "UHC" Expr.Text
    , emptyListDefault "YHC" Expr.Text
    ]
  )


buildInfoDefault :: Default s a
buildInfoDefault resolve = fields
  where
    fields = Map.fromList
      [ emptyListDefault "autogen-modules" Expr.Text
      , emptyListDefault "build-depends" ( generaliseDeclared dependency )
      , emptyListDefault "build-tool-depends" ( generaliseDeclared exeDependency )
      , emptyListDefault "build-tools"
          ( generaliseDeclared legacyExeDependency )
      , ( "buildable", Expr.BoolLit True )
      , emptyListDefault "c-sources" Expr.Text
      , emptyListDefault "cc-options" Expr.Text
      , ( "compiler-options", resolve ( PreludeDefault CompilerOptions ) )
      , emptyListDefault "cpp-options" Expr.Text
      , emptyListDefault "default-extensions" ( generaliseDeclared extension )
      , emptyOptionalDefault "default-language" ( generaliseDeclared language )
      , emptyListDefault "extra-framework-dirs" Expr.Text
      , emptyListDefault "extra-ghci-libraries" Expr.Text
      , emptyListDefault "extra-lib-dirs" Expr.Text
      , emptyListDefault "extra-libraries" Expr.Text
      , emptyListDefault "frameworks" Expr.Text
      , emptyListDefault "hs-source-dirs" Expr.Text
      , emptyListDefault "includes" Expr.Text
      , emptyListDefault "include-dirs" Expr.Text
      , emptyListDefault "install-includes" Expr.Text
      , emptyListDefault "js-sources" Expr.Text
      , emptyListDefault "ld-options" Expr.Text
      , emptyListDefault "other-extensions" ( generaliseDeclared extension )
      , emptyListDefault "other-languages" ( generaliseDeclared language )
      , emptyListDefault "other-modules" Expr.Text
      , emptyListDefault "pkgconfig-depends" ( generaliseDeclared pkgconfigDependency )
      , ( "profiling-options", resolve ( PreludeDefault CompilerOptions ) )
      , ( "shared-options", resolve ( PreludeDefault CompilerOptions ) )
      , ( "static-options", resolve ( PreludeDefault CompilerOptions ) )
      , emptyListDefault "mixins" ( generaliseDeclared mixin )
      , emptyListDefault "asm-options" Expr.Text
      , emptyListDefault "asm-sources" Expr.Text
      , emptyListDefault "cmm-options" Expr.Text
      , emptyListDefault "cmm-sources" Expr.Text
      , emptyListDefault "cxx-options" Expr.Text
      , emptyListDefault "cxx-sources" Expr.Text
      , emptyListDefault "virtual-modules" Expr.Text
      , emptyListDefault "extra-lib-flavours" Expr.Text
      , emptyListDefault "extra-bundled-libs" Expr.Text
      ]


libraryDefault :: Default s a
libraryDefault resolve = buildInfoDefault resolve <> specificFields
  where
    specificFields = Map.fromList
      [ emptyListDefault "exposed-modules" Expr.Text
      , emptyListDefault "other-modules" Expr.Text
      , emptyListDefault "reexported-modules"
          ( generaliseDeclared moduleReexport )
      , emptyListDefault "signatures" Expr.Text
      ]


benchmarkDefault :: Default s a
benchmarkDefault = buildInfoDefault


testSuiteDefault :: Default s a
testSuiteDefault = buildInfoDefault


executableDefault :: Default s a
executableDefault resolve = buildInfoDefault resolve <> specificFields
  where
    specificFields =
      Map.singleton "scope"
        ( Expr.App
            ( resolve PreludeConstructorsScope `Expr.Field` "Public" )
            ( Expr.RecordLit mempty )
        )


packageDefault :: Default s a
packageDefault resolve = fields
  where
    named name typ = Expr.Record
      ( Map.fromList
          [ ( "name"
            , Expr.Text
            )
          , ( name
            , Expr.Pi
                "config"
                ( Expr.Var "types" `Expr.Field` "Config" )
                ( generaliseDeclared typ )
            )
          ]
      )

    fields = Map.fromList
      [ textFieldDefault "author" ""
      , emptyListDefault "flags" ( generaliseDeclared flag )
      , emptyListDefault "benchmarks" ( named "benchmark" benchmark )
      , textFieldDefault "bug-reports" ""
      , emptyOptionalDefault "build-type"
          ( generaliseDeclared buildType )
      , ( "cabal-version"
        , Expr.App
            ( resolve PreludeV )
            ( Expr.TextLit ( Dhall.Core.Chunks [] "2.2" ) )
        )
      , textFieldDefault "category" ""
      , textFieldDefault "copyright" ""
      , textFieldDefault "data-dir" ""
      , emptyListDefault "data-files" Expr.Text
      , textFieldDefault "description" ""
      , emptyListDefault "executables" ( named "executable" executable )
      , emptyListDefault "extra-doc-files" Expr.Text
      , emptyListDefault "extra-source-files" Expr.Text
      , emptyListDefault "extra-tmp-files" Expr.Text
      , emptyListDefault "foreign-libraries" ( named "foreign-lib" foreignLibrary )
      , textFieldDefault "homepage" ""
      , emptyOptionalDefault "library"
          ( Expr.Pi
              "config"
              ( Expr.Var "types" `Expr.Field` "Config" )
              ( generaliseDeclared library )
          )
      , ( "license"
        , Expr.App
            ( resolve PreludeConstructorsLicense `Expr.Field` "AllRightsReserved" )
            ( Expr.RecordLit mempty )
        )
      , emptyListDefault "license-files" Expr.Text
      , textFieldDefault "maintainer" ""
      , textFieldDefault "package-url" ""
      , emptyListDefault "source-repos" ( generaliseDeclared sourceRepo )
      , textFieldDefault "stability" ""
      , emptyListDefault "sub-libraries" ( named "library" library )
      , textFieldDefault "synopsis" ""
      , emptyListDefault "test-suites" ( named "test-suite" testSuite )
      , emptyListDefault "tested-with"
          ( Expr.Record
              ( Map.fromList
                  [ ( "compiler", generaliseDeclared compilerFlavor )
                  , ( "version", generaliseDeclared versionRange )
                  ]
              )
          )
      , emptyListDefault "x-fields"
          ( Expr.Record
              ( Map.fromList
                  [ ( "_1", Expr.Text ), ( "_2", Expr.Text ) ]
              )
          )
      , emptyOptionalDefault "custom-setup"
          ( generaliseDeclared setupBuildInfo )
      ]


sourceRepoDefault :: Default s a
sourceRepoDefault resolve = fields
  where
    fields = Map.fromList
      [ emptyOptionalDefault "type" ( generaliseDeclared repoType )
      , emptyOptionalDefault "location" Expr.Text
      , emptyOptionalDefault "module" Expr.Text
      , emptyOptionalDefault "branch" Expr.Text
      , emptyOptionalDefault "tag" Expr.Text
      , emptyOptionalDefault "subdir" Expr.Text
      , ( "kind"
        , Expr.App
            ( resolve PreludeConstructorsRepoKind `Expr.Field` "RepoHead" )
            ( Expr.RecordLit mempty )
        )
      ]


data DefaultComparison s a
  = DefaultComparisonMatch
  | DefaultComparisonReplace ( Expr.Expr s a )
  deriving ( Show )


extractDefaultComparisonReplace
  :: DefaultComparison s a
  -> Maybe ( Expr.Expr s a )
extractDefaultComparisonReplace DefaultComparisonMatch =
  Nothing
extractDefaultComparisonReplace ( DefaultComparisonReplace expr ) =
  Just expr


nonDefaultFields
  :: ( Eq a )
  => Map.Map StrictText.Text ( Expr.Expr s a )
  -> Map.Map StrictText.Text ( Expr.Expr s a )
  -> Map.Map StrictText.Text ( Expr.Expr s a )
nonDefaultFields defs fields =
  let
    withoutDefaults = Map.difference fields defs
    compared = Map.intersectionWith compareToDefault defs fields
    changed = Map.mapMaybe extractDefaultComparisonReplace compared
  in
    withoutDefaults <> changed


compareToDefault :: ( Eq a ) => Expr.Expr s a -> Expr.Expr s a -> DefaultComparison s a
compareToDefault def expr | Dhall.Core.judgmentallyEqual def expr =
  DefaultComparisonMatch
compareToDefault _ expr =
  DefaultComparisonReplace expr


withDefault :: ( Eq a ) => KnownDefault -> Default s a -> Expr.Expr s a -> Expr.Expr s a
withDefault typ defs ( Expr.RecordLit fields ) =
  let
    nonDefaults = nonDefaultFields ( defs resolvePreludeVar ) fields
    name = StrictText.pack ( show typ )
  in
    if null nonDefaults
    then Expr.Var ( Expr.V "prelude" 0 ) `Expr.Field` "defaults" `Expr.Field` name
    else Expr.Prefer
           ( Expr.Var ( Expr.V "prelude" 0 ) `Expr.Field` "defaults" `Expr.Field` name )
           ( Expr.RecordLit nonDefaults )
withDefault _ _ expr =
  expr


newtype RecordInputType a =
  RecordInputType
    { _unRecordInputType ::
        Map.Map Dhall.Text ( Dhall.InputType a )
    }
  deriving ( Semigroup, Monoid )


instance Contravariant RecordInputType where
  contramap f ( RecordInputType map ) =
    RecordInputType ( fmap ( contramap f ) map )


recordField :: Dhall.Text -> Dhall.InputType a -> RecordInputType a
recordField k v =
  RecordInputType ( Map.singleton k v )


runRecordInputType :: RecordInputType a -> Dhall.InputType a
runRecordInputType ( RecordInputType m ) =
  Dhall.InputType
    { Dhall.embed =
        \a -> sortExpr ( Expr.RecordLit ( fmap ( \t -> Dhall.embed t a ) m ) )
    , Dhall.declared = sortExpr ( Expr.Record ( fmap Dhall.declared m ) )
    }


runRecordInputTypeWithDefault :: KnownDefault -> Default Dhall.Parser.Src Dhall.TypeCheck.X -> RecordInputType a -> Dhall.InputType a
runRecordInputTypeWithDefault typ def m =
  let
    Dhall.InputType embed declared = runRecordInputType m
  in
    Dhall.InputType ( withDefault typ def . embed ) declared


genericPackageDescriptionToDhall
  :: Dhall.InputType Cabal.GenericPackageDescription
genericPackageDescriptionToDhall =
  let
    named k v =
      listOf
        ( runRecordInputType
            ( mconcat
                [ fst >$< recordField "name" unqualComponentName
                , snd >$< recordField k v
                ]
            )
        )

  in
  runRecordInputTypeWithDefault Package packageDefault
    ( mconcat
        [ Cabal.packageDescription >$< packageDescriptionToRecord
        , recordField "flags" ( Cabal.genPackageFlags >$< ( listOf flag ) )
        , recordField "library" ( Cabal.condLibrary >$< maybeToDhall ( condTree library ) )
        , recordField "sub-libraries" ( Cabal.condSubLibraries >$< named "library" ( condTree library ) )
        , recordField "foreign-libraries" ( Cabal.condForeignLibs >$< named "foreign-lib" ( condTree foreignLibrary ) )
        , recordField "executables" ( Cabal.condExecutables >$< named "executable" ( condTree executable ) )
        , recordField "test-suites" ( Cabal.condTestSuites >$< named "test-suite" ( condTree testSuite ) )
        , recordField "benchmarks" ( Cabal.condBenchmarks >$< named "benchmark" ( condTree benchmark ) )
        ]
    )


packageDescriptionToRecord
  :: RecordInputType Cabal.PackageDescription
packageDescriptionToRecord =
  mconcat
    [ contramap Cabal.package packageIdentifierToRecord
    , recordField "source-repos" ( contramap Cabal.sourceRepos ( listOf sourceRepo ) )
    , recordField "cabal-version" ( contramap Cabal.specVersionRaw specVersion )
    , recordField "build-type" ( contramap Cabal.buildTypeRaw ( maybeToDhall buildType ) )
    , recordField "license" ( contramap Cabal.licenseRaw licenseToDhall )
    , recordField "license-files" ( contramap Cabal.licenseFiles ( listOf stringToDhall ) )
    , recordField "copyright" ( contramap Cabal.copyright stringToDhall )
    , recordField "maintainer" ( contramap Cabal.maintainer stringToDhall )
    , recordField "author" ( contramap Cabal.author stringToDhall )
    , recordField "stability" ( contramap Cabal.stability stringToDhall )
    , recordField "tested-with" ( contramap Cabal.testedWith ( listOf compiler ) )
    , recordField "homepage" ( contramap Cabal.homepage stringToDhall )
    , recordField "package-url" ( contramap Cabal.pkgUrl stringToDhall )
    , recordField "bug-reports" ( contramap Cabal.bugReports stringToDhall )
    , recordField "synopsis" ( contramap Cabal.synopsis stringToDhall )
    , recordField "description" ( contramap Cabal.description stringToDhall )
    , recordField "category" ( contramap Cabal.category stringToDhall )
    , recordField "custom-setup" ( contramap Cabal.setupBuildInfo ( maybeToDhall setupBuildInfo ) )
    , recordField "data-files" ( contramap Cabal.dataFiles ( listOf stringToDhall ) )
    , recordField "data-dir" ( contramap Cabal.dataDir stringToDhall )
    , recordField "extra-source-files" ( contramap Cabal.extraSrcFiles ( listOf stringToDhall ) )
    , recordField "extra-tmp-files" ( contramap Cabal.extraTmpFiles ( listOf stringToDhall ) )
    , recordField "extra-doc-files" ( contramap Cabal.extraDocFiles ( listOf stringToDhall ) )
    , recordField
        "x-fields"
        ( Cabal.customFieldsPD
            >$<
              listOf
                ( runRecordInputType
                    ( mconcat
                        [ fst >$< recordField "_1" stringToDhall
                        , snd >$< recordField "_2" stringToDhall
                        ]
                    )
                )
        )
    ]


packageIdentifierToRecord
  :: RecordInputType Cabal.PackageIdentifier
packageIdentifierToRecord =
  mconcat
    [ recordField "name" ( contramap Cabal.pkgName packageNameToDhall )
    , recordField "version" ( contramap Cabal.pkgVersion versionToDhall )
    ]


packageNameToDhall :: Dhall.InputType Cabal.PackageName
packageNameToDhall =
  contramap Cabal.unPackageName stringToDhall


versionToDhall :: Dhall.InputType Cabal.Version
versionToDhall =
  Dhall.InputType
    { Dhall.embed =
        Expr.App ( Expr.Var "prelude" `Expr.Field` "v" )
          . Dhall.embed stringToDhall
          . show
          . Cabal.disp
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "Version"
    }


stringToDhall :: Dhall.InputType String
stringToDhall =
  contramap StrictText.pack Dhall.inject

licenseToDhall :: Dhall.InputType (Either SPDX.License Cabal.License)
licenseToDhall =
  Dhall.InputType
    { Dhall.embed = \l ->
        case l of
          Right ( Cabal.GPL v ) ->
            license "GPL" ( Dhall.embed ( maybeToDhall versionToDhall ) v )
          Right ( Cabal.AGPL v ) ->
            license "AGPL" ( Dhall.embed ( maybeToDhall versionToDhall ) v )
          Right ( Cabal.LGPL v ) ->
            license "LGPL" ( Dhall.embed ( maybeToDhall versionToDhall ) v )
          Right Cabal.BSD2 ->
            license "BSD2" ( Expr.RecordLit mempty )
          Right Cabal.BSD3 ->
            license "BSD3" ( Expr.RecordLit mempty )
          Right Cabal.BSD4 ->
            license "BSD4" ( Expr.RecordLit mempty )
          Right Cabal.MIT ->
            license "MIT" ( Expr.RecordLit mempty )
          Right Cabal.ISC ->
            license "ISC" ( Expr.RecordLit mempty )
          Right ( Cabal.MPL v ) ->
            license "MPL" ( Dhall.embed versionToDhall v )
          Right ( Cabal.Apache v ) ->
            license "Apache" ( Dhall.embed ( maybeToDhall versionToDhall ) v )
          Right Cabal.PublicDomain ->
            license "PublicDomain" ( Expr.RecordLit mempty )
          Right Cabal.AllRightsReserved ->
            license "AllRightsReserved" ( Expr.RecordLit mempty )
          -- Note: SPDX.NONE is what Cabal reports for a file without
          -- a 'license' field, even for pre-2.2 spec versions.
          Left SPDX.NONE ->
            license "AllRightsReserved" ( Expr.RecordLit mempty )
          Right Cabal.UnspecifiedLicense ->
            license "Unspecified" ( Expr.RecordLit mempty )
          Right ( Cabal.UnknownLicense "UnspecifiedLicense" ) ->
            license "Unspecified" ( Expr.RecordLit mempty )
          Right Cabal.OtherLicense ->
            license "Other" ( Expr.RecordLit mempty )
          Left ( SPDX.License x ) ->
            license "SPDX" ( Dhall.embed spdxLicenseExpressionToDhall x )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "License"
    }
  where
    license name =
      Expr.App
        ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "Licenses" `Expr.Field` name )

spdxLicenseExpressionToDhall :: Dhall.InputType SPDX.LicenseExpression
spdxLicenseExpressionToDhall =
    Dhall.InputType
    { Dhall.embed =
        let
          go lexp = case lexp of
            SPDX.ELicense ( SPDX.ELicenseId ident ) exceptionMay ->
              Expr.App
                ( Expr.App
                    ( Expr.Var "prelude" `Expr.Field` "SPDX" `Expr.Field` "license" )
                    ( Dhall.embed spdxLicenseIdToDhall ident )
                )
                ( Dhall.embed ( maybeToDhall spdxLicenseExceptionIdToDhall ) exceptionMay )
            SPDX.ELicense (SPDX.ELicenseIdPlus ident) exceptionMay ->
              Expr.App
                ( Expr.App
                    ( Expr.Var "prelude" `Expr.Field` "SPDX" `Expr.Field` "licenseVersionOrLater" )
                    ( Dhall.embed spdxLicenseIdToDhall ident )
                )
                ( Dhall.embed ( maybeToDhall spdxLicenseExceptionIdToDhall ) exceptionMay )
            SPDX.ELicense (SPDX.ELicenseRef ref) exceptionMay ->
              case SPDX.licenseDocumentRef ref of
                Nothing ->
                  Expr.App
                    ( Expr.App
                        ( Expr.Var "prelude" `Expr.Field` "SPDX" `Expr.Field` "ref" )
                        ( Dhall.embed stringToDhall ( SPDX.licenseRef ref ) )
                    )
                    ( Dhall.embed ( maybeToDhall spdxLicenseExceptionIdToDhall ) exceptionMay )
                Just file ->
                  Expr.App
                    ( Expr.App
                        ( Expr.App
                            ( Expr.Var "prelude" `Expr.Field` "SPDX" `Expr.Field` "refWithFile" )
                            ( Dhall.embed stringToDhall ( SPDX.licenseRef ref ) )
                        )
                        ( Dhall.embed stringToDhall file )
                    )
                    ( Dhall.embed ( maybeToDhall spdxLicenseExceptionIdToDhall ) exceptionMay )
            SPDX.EOr a b ->
              Expr.App
                ( Expr.App
                    ( Expr.Var "prelude" `Expr.Field` "SPDX" `Expr.Field` "or" )
                    ( go a )
                )
                ( go b )
            SPDX.EAnd a b ->
              Expr.App
                ( Expr.App
                    ( Expr.Var "prelude" `Expr.Field` "SPDX" `Expr.Field` "and" )
                    ( go a )
                )
                ( go b )
        in go
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "SPDX"
    }

spdxLicenseIdToDhall :: Dhall.InputType SPDX.LicenseId
spdxLicenseIdToDhall =
  Dhall.InputType
    { Dhall.embed = \ident ->
        Expr.App
          ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "LicenseId" `Expr.Field` identName ident )
          ( Expr.RecordLit mempty )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "LicenseId"
    }

  where

  identName :: SPDX.LicenseId -> StrictText.Text
  identName e =
    StrictText.pack ( show e )

spdxLicenseExceptionIdToDhall :: Dhall.InputType SPDX.LicenseExceptionId
spdxLicenseExceptionIdToDhall =
  Dhall.InputType
    { Dhall.embed = \ident ->
        Expr.App
          ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "LicenseExceptionId" `Expr.Field` identName ident )
          ( Expr.RecordLit mempty )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "LicenseExceptionId"
    }

  where

  identName :: SPDX.LicenseExceptionId -> StrictText.Text
  identName e =
    StrictText.pack ( show e )

newtype Union a =
  Union
    { _unUnion ::
        ( a ->
          ( First ( Dhall.Text, DhallExpr )
          , Map.Map Dhall.Text DhallExpr
          )
        , Map.Map Dhall.Text DhallExpr
        )
    }
  deriving ( Semigroup, Monoid )


runUnion :: ( HasCallStack, Show a ) => Union a -> Dhall.InputType a
runUnion ( Union ( f, t ) ) =
  Dhall.InputType
    { Dhall.embed =
        \a ->
          case f a of
            ( First Nothing, _ ) ->
              error $ "Union did not match anything. Given " ++ show a

            ( First ( Just ( k, v ) ), alts ) ->
              Expr.UnionLit k v alts
    , Dhall.declared =
        sortExpr ( Expr.Union t )
    }


unionAlt :: Dhall.Text -> ( a -> Maybe b ) -> Dhall.InputType b -> Union a
unionAlt k f t =
  Union
    ( \a ->
        case f a of
          Nothing ->
            ( mempty, Map.singleton k ( Dhall.declared t ) )

          Just _ ->
            ( First ( fmap ( \b -> ( k, Dhall.embed t b ) ) ( f a ) ), mempty )
    , Map.singleton k ( Dhall.declared t )
    )


maybeToDhall :: Dhall.InputType a -> Dhall.InputType ( Maybe a )
maybeToDhall t =
  Dhall.InputType
    { Dhall.embed =
        \a -> case a of
            Nothing -> Expr.App Expr.None (Dhall.declared t)
            Just x  -> Expr.Some ( Dhall.embed t x )
    , Dhall.declared = Expr.App Expr.Optional ( Dhall.declared t )
    }


listOf :: Dhall.InputType a -> Dhall.InputType [ a ]
listOf t =
  Dhall.InputType
    { Dhall.embed =
        \a ->
          Expr.ListLit
            ( foldl ( \_ _ -> Nothing ) ( Just ( Dhall.declared t ) ) a )
            ( foldMap ( pure . Dhall.embed t ) a )
    , Dhall.declared = Expr.App Expr.List ( Dhall.declared t )
    }


compiler :: Dhall.InputType ( Cabal.CompilerFlavor, Cabal.VersionRange )
compiler =
  runRecordInputType
    ( mconcat
        [ recordField "compiler" ( contramap fst compilerFlavor )
        , recordField "version" ( contramap snd versionRange )
        ]
    )


compilerFlavor :: Dhall.InputType Cabal.CompilerFlavor
compilerFlavor =
  let
    constructor k v =
      Expr.App ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "Compilers" `Expr.Field` k ) v

  in
  Dhall.InputType
    { Dhall.embed = \case
        Cabal.Eta ->
          constructor "Eta" ( Expr.RecordLit mempty )

        Cabal.GHC ->
          constructor "GHC" ( Expr.RecordLit mempty )

        Cabal.GHCJS ->
          constructor "GHCJS" ( Expr.RecordLit mempty )

        Cabal.HBC ->
          constructor "HBC" ( Expr.RecordLit mempty )

        Cabal.HaskellSuite v ->
          constructor "HaskellSuite"
          ( Expr.Record ( Map.singleton "_1" ( dhallString v ) ) )

        Cabal.Helium ->
          constructor "Helium" ( Expr.RecordLit mempty )

        Cabal.Hugs ->
          constructor "Hugs" ( Expr.RecordLit mempty )

        Cabal.JHC ->
          constructor "JHC" ( Expr.RecordLit mempty )

        Cabal.LHC ->
          constructor "LHC" ( Expr.RecordLit mempty )

        Cabal.NHC ->
          constructor "NHC" ( Expr.RecordLit mempty )

        Cabal.OtherCompiler v ->
          constructor "OtherCompiler"
          ( Expr.Record ( Map.singleton "_1" ( dhallString v ) ) )

        Cabal.UHC ->
          constructor "UHC" ( Expr.RecordLit mempty )

        Cabal.YHC ->
          constructor "YHC" ( Expr.RecordLit mempty )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "Compiler"
    }


versionRange :: Dhall.InputType Cabal.VersionRange
versionRange =
  Dhall.InputType
    { Dhall.embed =
        \versionRange0 ->
          let
            go = Cabal.foldVersionRange
              -- AnyVersion
              ( Expr.Var "prelude" `Expr.Field` "anyVersion" )
              -- ThisVersion
              ( \v -> Expr.App
                  ( Expr.Var "prelude" `Expr.Field` "thisVersion" )
                  ( Dhall.embed versionToDhall v )
              )
              -- LaterVersion
              ( \v -> Expr.App
                  ( Expr.Var "prelude" `Expr.Field` "laterVersion" )
                  ( Dhall.embed versionToDhall v )
              )
              -- EarlierVersion
              ( \v -> Expr.App
                  ( Expr.Var "prelude" `Expr.Field` "earlierVersion" )
                  ( Dhall.embed versionToDhall v )
              )
              -- UnionVersionRanges
              ( \a b -> Expr.App
                  ( Expr.App
                      ( Expr.Var "prelude" `Expr.Field` "unionVersionRanges" )
                      a
                  )
                  b
              )
              -- IntersectVersionRanges
              ( \a b -> Expr.App
                  ( Expr.App
                      ( Expr.Var "prelude" `Expr.Field` "intersectVersionRanges" )
                      a
                  )
                  b
              )

          in
          go ( Cabal.fromVersionIntervals ( Cabal.toVersionIntervals versionRange0 ) )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "VersionRange"
    }


sourceRepo :: Dhall.InputType Cabal.SourceRepo
sourceRepo =
  ( runRecordInputTypeWithDefault SourceRepo sourceRepoDefault
      ( mconcat
          [ recordField "kind" ( contramap Cabal.repoKind repoKind )
          , recordField "type" ( contramap Cabal.repoType ( maybeToDhall repoType ) )
          , recordField "location" ( contramap Cabal.repoLocation ( maybeToDhall stringToDhall ) )
          , recordField "module" ( contramap Cabal.repoModule ( maybeToDhall stringToDhall ) )
          , recordField "branch" ( contramap Cabal.repoBranch ( maybeToDhall stringToDhall ) )
          , recordField "tag" ( contramap Cabal.repoTag ( maybeToDhall stringToDhall ) )
          , recordField "subdir" ( contramap Cabal.repoSubdir ( maybeToDhall stringToDhall ) )
          ]
      )
  )
  { Dhall.declared =
      Expr.Var "types" `Expr.Field` "SourceRepo"
  }


repoKind :: Dhall.InputType Cabal.RepoKind
repoKind =
  Dhall.InputType
    { Dhall.embed = \case
        Cabal.RepoThis ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "RepoKind" `Expr.Field` "RepoThis" )
            ( Expr.RecordLit mempty )
        Cabal.RepoHead ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "RepoKind" `Expr.Field` "RepoHead" )
            ( Expr.RecordLit mempty )
        Cabal.RepoKindUnknown str ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "RepoKind" `Expr.Field` "RepoThis" )
            ( Expr.RecordLit ( Map.singleton "_1" ( dhallString str ) ) )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "RepoKind"
    }


repoType :: Dhall.InputType Cabal.RepoType
repoType =
  Dhall.InputType
    { Dhall.embed = \case
        Cabal.Darcs ->
          Expr.App ( constr "Darcs" ) ( Expr.RecordLit mempty )
        Cabal.Git ->
          Expr.App ( constr "Git" ) ( Expr.RecordLit mempty )
        Cabal.SVN ->
          Expr.App ( constr "SVN" ) ( Expr.RecordLit mempty )
        Cabal.CVS ->
          Expr.App ( constr "CVS" ) ( Expr.RecordLit mempty )
        Cabal.Mercurial ->
          Expr.App ( constr "Mercurial" ) ( Expr.RecordLit mempty )
        Cabal.GnuArch ->
          Expr.App ( constr "GnuArch" ) ( Expr.RecordLit mempty )
        Cabal.Monotone ->
          Expr.App ( constr "Monotone" ) ( Expr.RecordLit mempty )
        Cabal.Bazaar ->
          Expr.App ( constr "Bazaar" ) ( Expr.RecordLit mempty )
        Cabal.OtherRepoType str ->
          Expr.App
            ( constr "OtherRepoType" )
            ( Expr.RecordLit ( Map.singleton "_1" ( dhallString str ) ) )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "RepoType"
    }
  where
    constr name =
      Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "RepoType" `Expr.Field` name


specVersion :: Dhall.InputType ( Either Cabal.Version Cabal.VersionRange )
specVersion =
  Dhall.InputType
    { Dhall.embed = either ( Dhall.embed versionToDhall ) ( error "Only exact cabal-versions are supported" )
    , Dhall.declared = Dhall.declared versionToDhall
    }


buildType :: Dhall.InputType Cabal.BuildType
buildType =
  Dhall.InputType
    { Dhall.embed = \case
        Cabal.Simple ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "BuildTypes" `Expr.Field` "Simple" )
            ( Expr.RecordLit mempty )

        Cabal.Configure ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "BuildTypes" `Expr.Field` "Configure" )
            ( Expr.RecordLit mempty )

        Cabal.Custom ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "BuildTypes" `Expr.Field` "Custom" )
            ( Expr.RecordLit mempty )

        Cabal.Make ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "BuildTypes" `Expr.Field` "Make" )
            ( Expr.RecordLit mempty )

    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "BuildType"
    }


setupBuildInfo :: Dhall.InputType Cabal.SetupBuildInfo
setupBuildInfo =
  ( runRecordInputType
      ( mconcat
          [ recordField "setup-depends" ( contramap Cabal.setupDepends ( listOf dependency ) )
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` "CustomSetup"
    }


dependency :: Dhall.InputType Cabal.Dependency
dependency =
  runRecordInputType
    ( mconcat
        [ recordField "package" ( contramap ( \( Cabal.Dependency p _ ) -> p ) packageNameToDhall )
        , recordField "bounds" ( contramap ( \( Cabal.Dependency _ a ) -> a ) versionRange )
        ]
    )


flag :: Dhall.InputType Cabal.Flag
flag =
  runRecordInputType
    ( mconcat
        [ recordField "name" ( contramap Cabal.flagName flagName )
        , recordField "default" ( contramap Cabal.flagDefault Dhall.inject )
        , recordField "description" ( contramap Cabal.flagDescription stringToDhall )
        , recordField "manual" ( contramap Cabal.flagManual Dhall.inject )
        ]
    )


flagName :: Dhall.InputType Cabal.FlagName
flagName =
  contramap Cabal.unFlagName stringToDhall


library :: Dhall.InputType Cabal.Library
library =
  ( runRecordInputTypeWithDefault Library libraryDefault
      ( mconcat
          [ contramap Cabal.libBuildInfo buildInfoRecord
          , recordField
              "exposed-modules"
              ( contramap Cabal.exposedModules ( listOf moduleName ) )
          , recordField
              "reexported-modules"
              ( contramap Cabal.reexportedModules ( listOf moduleReexport ) )
          , recordField
              "signatures"
              ( contramap Cabal.signatures ( listOf moduleName ) )
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` "Library"
    }


unifyCondTree
  :: ( Monoid a )
  => Cabal.CondTree v x a
  -> ConfigTree ( Cabal.Condition v ) a
unifyCondTree =
  let
    branch
      :: ( Monoid a )
      => Cabal.CondBranch v x a
      -> ConfigTree ( Cabal.Condition v ) a
    branch ( Cabal.CondBranch cond true false ) =
      Branch cond ( tree true ) ( maybe mempty tree false )

    tree
      :: ( Monoid a )
      => Cabal.CondTree v x a
      -> ConfigTree ( Cabal.Condition v ) a
    tree ( Cabal.CondNode acc _ branches) =
      return acc `mappend` foldMap branch branches
  in
  tree


condTree
  :: ( Monoid a )
  => Dhall.InputType a
  -> Dhall.InputType ( Cabal.CondTree Cabal.ConfVar x a )
condTree t =
  let
    go = \case
      Leaf a ->
        Dhall.embed t a

      Branch cond a b ->
        Expr.BoolIf
          ( Dhall.embed condBranchCondition cond )
          ( go a )
          ( go b )

    configRecord =
      Expr.Var "types" `Expr.Field` "Config"

  in
  Dhall.InputType
    { Dhall.embed =
        Expr.Lam "config" configRecord
          . go
          . unifyCondTree
    , Dhall.declared =
        Expr.Pi "_" configRecord ( Dhall.declared t )
    }


moduleName :: Dhall.InputType Cabal.ModuleName
moduleName =
  contramap ( show . Cabal.disp ) stringToDhall


condBranchCondition :: Dhall.InputType (Cabal.Condition Cabal.ConfVar)
condBranchCondition =
  Dhall.InputType
    { Dhall.declared = Expr.Bool
    , Dhall.embed =
        \a ->
          case a of
            Cabal.Var ( Cabal.OS os0 ) ->
              Expr.App ( Expr.Field ( Expr.Var "config" ) "os" ) ( Dhall.embed os os0 )

            Cabal.Var ( Cabal.Arch arch0 ) ->
              Expr.App ( Expr.Field ( Expr.Var "config" ) "arch" ) ( Dhall.embed arch arch0 )

            Cabal.Var ( Cabal.Flag flagName0 ) ->
              Expr.App ( Expr.Field ( Expr.Var "config" ) "flag" ) ( Dhall.embed flagName flagName0 )

            Cabal.Var ( Cabal.Impl c v ) ->
              Expr.App ( Expr.App ( Expr.Field ( Expr.Var "config" ) "impl" ) ( Dhall.embed compilerFlavor c ) ) ( Dhall.embed versionRange v )

            Cabal.Lit b ->
              Expr.BoolLit b

            Cabal.CNot c ->
              Expr.BoolEQ ( Expr.BoolLit False ) ( Dhall.embed condBranchCondition c )

            Cabal.CAnd a b ->
              Expr.BoolAnd ( Dhall.embed condBranchCondition a ) ( Dhall.embed condBranchCondition b )

            Cabal.COr a b ->
              Expr.BoolOr ( Dhall.embed condBranchCondition a ) ( Dhall.embed condBranchCondition b )
    }


os :: Dhall.InputType Cabal.OS
os =
  Dhall.InputType
    { Dhall.embed = \case
        Cabal.Linux ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "Linux" )
            ( Expr.RecordLit mempty )

        Cabal.Windows ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "Windows" )
            ( Expr.RecordLit mempty )

        Cabal.OSX ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "OSX" )
            ( Expr.RecordLit mempty )

        Cabal.FreeBSD ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "FreeBSD" )
            ( Expr.RecordLit mempty )

        Cabal.OpenBSD ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "OpenBSD" )
            ( Expr.RecordLit mempty )

        Cabal.NetBSD ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "NetBSD" )
            ( Expr.RecordLit mempty )

        Cabal.DragonFly ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "DragonFly" )
            ( Expr.RecordLit mempty )

        Cabal.Solaris ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "Solaris" )
            ( Expr.RecordLit mempty )

        Cabal.AIX ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "AIX" )
            ( Expr.RecordLit mempty )

        Cabal.HPUX ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "HPUX" )
            ( Expr.RecordLit mempty )

        Cabal.IRIX ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "IRIX" )
            ( Expr.RecordLit mempty )

        Cabal.HaLVM ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "HaLVM" )
            ( Expr.RecordLit mempty )

        Cabal.Hurd ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "Hurd" )
            ( Expr.RecordLit mempty )

        Cabal.IOS ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "IOS" )
            ( Expr.RecordLit mempty )

        Cabal.Android ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "Android" )
            ( Expr.RecordLit mempty )

        Cabal.Ghcjs ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "Ghcjs" )
            ( Expr.RecordLit mempty )

        Cabal.OtherOS os ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OSs" `Expr.Field` "OtherOS" )
            ( Expr.RecordLit ( Map.singleton "_1" ( dhallString os ) ) )

    , Dhall.declared =
        Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "OS"
    }


arch :: Dhall.InputType Cabal.Arch
arch =
  runUnion
    ( mconcat
        [ unionAlt "I386" ( \x -> case x of Cabal.I386 -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "X86_64" ( \x -> case x of Cabal.X86_64 -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "PPC" ( \x -> case x of Cabal.PPC -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "PPC64" ( \x -> case x of Cabal.PPC64 -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Sparc" ( \x -> case x of Cabal.Sparc -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Arm" ( \x -> case x of Cabal.Arm -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Mips" ( \x -> case x of Cabal.Mips -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "SH" ( \x -> case x of Cabal.SH -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "IA64" ( \x -> case x of Cabal.IA64 -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "S390" ( \x -> case x of Cabal.S390 -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Alpha" ( \x -> case x of Cabal.Alpha -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Hppa" ( \x -> case x of Cabal.Hppa -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Rs6000" ( \x -> case x of Cabal.Rs6000 -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "M68k" ( \x -> case x of Cabal.M68k -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Vax" ( \x -> case x of Cabal.Vax -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "JavaScript" ( \x -> case x of Cabal.JavaScript -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "OtherArch" ( \x -> case x of Cabal.OtherArch s -> Just s ; _ -> Nothing ) ( runRecordInputType ( recordField "_1" stringToDhall ) )
        ]
    )


buildInfoRecord :: RecordInputType Cabal.BuildInfo
buildInfoRecord =
  mconcat
    [ recordField "buildable" ( contramap Cabal.buildable Dhall.inject )
    , recordField "build-tools" ( contramap Cabal.buildTools ( listOf legacyExeDependency ) )
    , recordField "build-tool-depends" ( contramap Cabal.buildToolDepends ( listOf exeDependency ) )
    , recordField "cpp-options" ( contramap Cabal.cppOptions ( listOf stringToDhall ) )
    , recordField "cc-options" ( contramap Cabal.ccOptions ( listOf stringToDhall ) )
    , recordField "ld-options" ( contramap Cabal.ldOptions ( listOf stringToDhall ) )
    , recordField "pkgconfig-depends" ( contramap Cabal.pkgconfigDepends ( listOf pkgconfigDependency ) )
    , recordField "frameworks" ( contramap Cabal.frameworks ( listOf stringToDhall ) )
    , recordField "extra-framework-dirs" ( contramap Cabal.extraFrameworkDirs ( listOf stringToDhall ) )
    , recordField "c-sources" ( contramap Cabal.cSources ( listOf stringToDhall ) )
    , recordField "js-sources" ( contramap Cabal.jsSources ( listOf stringToDhall ) )
    , recordField "hs-source-dirs" ( contramap Cabal.hsSourceDirs ( listOf stringToDhall ) )
    , recordField "other-modules" ( contramap Cabal.otherModules ( listOf moduleName ) )
    , recordField "autogen-modules" ( contramap Cabal.autogenModules ( listOf moduleName ) )
    , recordField "default-language" ( contramap Cabal.defaultLanguage ( maybeToDhall language ) )
    , recordField "other-languages" ( contramap Cabal.otherLanguages ( listOf language ) )
    , recordField "default-extensions" ( Cabal.defaultExtensions >$< listOf extension )
    , recordField "other-extensions" ( Cabal.otherExtensions >$< listOf extension )
    , recordField "extra-libraries" ( Cabal.extraLibs >$< listOf stringToDhall )
    , recordField "extra-ghci-libraries" ( Cabal.extraGHCiLibs >$< listOf stringToDhall )
    , recordField "extra-lib-dirs" ( Cabal.extraLibDirs >$< listOf stringToDhall )
    , recordField "include-dirs" ( Cabal.includeDirs >$< listOf stringToDhall )
    , recordField "includes" ( Cabal.includes >$< listOf stringToDhall )
    , recordField "install-includes" ( Cabal.installIncludes >$< listOf stringToDhall )
    , recordField "compiler-options" ( Cabal.options >$< compilerOptions )
    , recordField "profiling-options" ( Cabal.profOptions >$< compilerOptions )
    , recordField "shared-options" ( Cabal.sharedOptions >$< compilerOptions )
    , recordField "static-options" ( Cabal.staticOptions >$< compilerOptions )
    , recordField "build-depends" ( Cabal.targetBuildDepends >$< listOf dependency )
    , recordField "mixins" ( Cabal.mixins >$< listOf mixin )
    , recordField "asm-options" ( Cabal.asmOptions >$< listOf stringToDhall)
    , recordField "asm-sources" ( Cabal.asmSources >$< listOf stringToDhall)
    , recordField "cmm-options" ( Cabal.cmmOptions >$< listOf stringToDhall )
    , recordField "cmm-sources" ( Cabal.cmmSources >$< listOf stringToDhall )
    , recordField "cxx-options" ( Cabal.cxxOptions >$< listOf stringToDhall )
    , recordField "cxx-sources" ( Cabal.cxxSources >$< listOf stringToDhall)
    , recordField "virtual-modules" ( Cabal.virtualModules >$< listOf moduleName )
    , recordField "extra-lib-flavours" ( Cabal.extraLibFlavours >$< listOf stringToDhall )
    , recordField "extra-bundled-libs" ( Cabal.extraBundledLibs >$< listOf stringToDhall )
    ]


moduleReexport :: Dhall.InputType Cabal.ModuleReexport
moduleReexport =
  runRecordInputType
    ( mconcat
        [ recordField "original"
             ( ( \a -> ( Cabal.moduleReexportOriginalPackage a, Cabal.moduleReexportOriginalName a ) ) >$<
                runRecordInputType
                 ( mconcat
                     [ recordField "package" ( fst >$< maybeToDhall packageNameToDhall )
                     , recordField "name" ( snd >$< moduleName )
                     ]
                 )
             )
        , recordField "name" ( Cabal.moduleReexportName >$< moduleName )
        ]
    )


legacyExeDependency :: Dhall.InputType Cabal.LegacyExeDependency
legacyExeDependency =
  runRecordInputType
    ( mconcat
        [ recordField "exe" ( ( \( Cabal.LegacyExeDependency exe _ ) -> exe ) >$< stringToDhall )
        , recordField "version" ( ( \( Cabal.LegacyExeDependency _ version ) -> version ) >$< versionRange )
        ]
    )

exeDependency :: Dhall.InputType Cabal.ExeDependency
exeDependency =
  runRecordInputType
    ( mconcat
        [ recordField "package" ( ( \( Cabal.ExeDependency packageName _ _ ) -> packageName ) >$< packageNameToDhall )
        , recordField "component" ( ( \( Cabal.ExeDependency _ component _ ) -> component ) >$< unqualComponentName )
        , recordField "version" ( ( \( Cabal.ExeDependency _ _ version ) -> version ) >$< versionRange )
        ]
    )


unqualComponentName :: Dhall.InputType Cabal.UnqualComponentName
unqualComponentName =
  show . Cabal.disp >$< stringToDhall


pkgconfigDependency :: Dhall.InputType Cabal.PkgconfigDependency
pkgconfigDependency =
  runRecordInputType
    ( mconcat
        [ recordField "name" ( ( \( Cabal.PkgconfigDependency a _version ) -> a ) >$< pkgconfigName )
        , recordField "version" ( ( \( Cabal.PkgconfigDependency _name a ) -> a ) >$< versionRange )
        ]
    )


pkgconfigName :: Dhall.InputType Cabal.PkgconfigName
pkgconfigName =
  show . Cabal.disp >$< stringToDhall


language :: Dhall.InputType Cabal.Language
language =
  ( runUnion
      ( mconcat
          [ unionAlt "Haskell2010" ( \x -> case x of Cabal.Haskell2010 -> Just () ; _ -> Nothing ) Dhall.inject
          , unionAlt "UnknownLanguage" ( \x -> case x of Cabal.UnknownLanguage s -> Just s ; _ -> Nothing ) ( runRecordInputType ( recordField "_1" stringToDhall ) )
          , unionAlt "Haskell98" ( \x -> case x of Cabal.Haskell98 -> Just () ; _ -> Nothing ) Dhall.inject
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` "Language"
    }

extension :: Dhall.InputType Cabal.Extension
extension =
  Dhall.InputType
    { Dhall.embed =
        \a ->
          case a of
            Cabal.EnableExtension ext ->
              extWith True ext

            Cabal.DisableExtension ext ->
              extWith False ext

            _ ->
              error "Unknown extension"
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "Extension"
    }

  where

  extName :: Cabal.KnownExtension -> StrictText.Text
  extName e =
    StrictText.pack ( show e )

  extWith trueFalse ext =
    Expr.App
      ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "Extensions" `Expr.Field` extName ext )
      ( Expr.BoolLit trueFalse )


compilerOptions :: Dhall.InputType [ ( Cabal.CompilerFlavor, [ String ] ) ]
compilerOptions =
  Dhall.InputType
    { Dhall.embed = \xs ->
        withDefault CompilerOptions compilerOptionsDefault
          ( Expr.RecordLit
              ( Map.fromList
                  ( map
                      ( \( c, opts ) ->
                          ( StrictText.pack ( show c )
                          , Expr.ListLit ( Just Expr.Text ) ( dhallString <$> Seq.fromList opts )
                          )
                      )
                      xs
                  )
              )
          )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "CompilerOptions"
    }


mixin :: Dhall.InputType Cabal.Mixin
mixin =
  ( runRecordInputType
      ( mconcat
          [ recordField "package" ( Cabal.mixinPackageName >$< packageNameToDhall )
          , recordField "renaming" ( Cabal.mixinIncludeRenaming >$< includeRenaming )
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` "Mixin"

    }


includeRenaming :: Dhall.InputType Cabal.IncludeRenaming
includeRenaming =
  runRecordInputType
    ( mconcat
        [ recordField "provides" ( Cabal.includeProvidesRn >$< moduleRenaming )
        , recordField "requires" ( Cabal.includeRequiresRn >$< moduleRenaming )
        ]
    )


moduleRenaming :: Dhall.InputType Cabal.ModuleRenaming
moduleRenaming =
  Dhall.InputType
    { Dhall.embed =
        \a ->
          case a of
            Cabal.ModuleRenaming renamed ->
              Expr.App
                ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "ModuleRenaming" `Expr.Field` "renaming" )
                ( Expr.ListLit
                    Nothing
                    ( fmap
                        (\ ( src, dst ) ->
                           Expr.RecordLit
                             ( Map.fromList
                                 [ ( "rename", Dhall.embed moduleName src )
                                 , ( "to", Dhall.embed moduleName dst )
                                 ]
                             )
                        )
                        ( Seq.fromList renamed )
                    )
                )
            Cabal.DefaultRenaming ->
              Expr.App
                ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "ModuleRenaming" `Expr.Field` "default" )
                ( Expr.RecordLit mempty )
            Cabal.HidingRenaming hidden ->
              Expr.App
                ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "ModuleRenaming" `Expr.Field` "hiding" )
                ( Expr.ListLit
                    Nothing
                    ( Dhall.embed moduleName <$> Seq.fromList hidden )
                )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "ModuleRenaming"
    }


benchmark :: Dhall.InputType Cabal.Benchmark
benchmark =
  (  runRecordInputTypeWithDefault Benchmark benchmarkDefault
       ( mconcat
           [ recordField "main-is" ( ( \( Cabal.BenchmarkExeV10 _ s ) -> s ) . Cabal.benchmarkInterface >$< stringToDhall )
           , Cabal.benchmarkBuildInfo >$< buildInfoRecord
           ]
       )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` "Benchmark"
    }


testSuite :: Dhall.InputType Cabal.TestSuite
testSuite =
  ( runRecordInputTypeWithDefault TestSuite testSuiteDefault
      ( mconcat
          [ recordField "type" ( Cabal.testInterface >$< testSuiteInterface )
          , Cabal.testBuildInfo >$< buildInfoRecord
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` "TestSuite"
    }


testSuiteInterface :: Dhall.InputType Cabal.TestSuiteInterface
testSuiteInterface =
  runUnion
    ( mconcat
        [ unionAlt
            "exitcode-stdio"
            ( \x ->
                case x of
                  Cabal.TestSuiteExeV10 _ main ->
                    Just main

                  _ ->
                    Nothing
            )
            ( runRecordInputType ( recordField "main-is" stringToDhall ) )
        , unionAlt
            "detailed"
            ( \x ->
                case x of
                  Cabal.TestSuiteLibV09 _ m ->
                    Just m

                  _ ->
                    Nothing
            )
            ( runRecordInputType ( recordField "module" moduleName ) )
        ]
    )


executable :: Dhall.InputType Cabal.Executable
executable =
  ( runRecordInputTypeWithDefault Executable executableDefault
      ( mconcat
          [ recordField "main-is" ( Cabal.modulePath >$< stringToDhall )
          , recordField "scope" ( Cabal.exeScope >$< executableScope )
          , Cabal.buildInfo >$< buildInfoRecord
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` "Executable"
    }


executableScope :: Dhall.InputType Cabal.ExecutableScope
executableScope =
  Dhall.InputType
    { Dhall.embed = \case
        Cabal.ExecutablePublic ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "Scopes" `Expr.Field` "Public" )
            ( Expr.RecordLit mempty )
        Cabal.ExecutablePrivate ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "scopes" `Expr.Field` "Private" )
            ( Expr.RecordLit mempty )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "Scope"
    }


foreignLibrary :: Dhall.InputType Cabal.ForeignLib
foreignLibrary =
  ( runRecordInputType
      ( mconcat
          [ recordField "type" ( Cabal.foreignLibType >$< foreignLibType )
          , recordField "options" ( Cabal.foreignLibOptions >$< ( listOf foreignLibOption ) )
          , Cabal.foreignLibBuildInfo >$< buildInfoRecord
          , recordField "lib-version-info" ( Cabal.foreignLibVersionInfo >$< maybeToDhall versionInfo )
          , recordField "lib-version-linux" ( Cabal.foreignLibVersionLinux >$< maybeToDhall versionToDhall )
          , recordField "mod-def-files" ( Cabal.foreignLibModDefFile >$< listOf stringToDhall )
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` "ForeignLibrary"
    }


versionInfo :: Dhall.InputType Cabal.LibVersionInfo
versionInfo =
  Cabal.libVersionInfoCRA >$<
  runRecordInputType
    ( mconcat
        [ recordField "current" ( ( \( a, _, _ ) -> fromIntegral a :: Natural ) >$< ( Dhall.inject ) )
        , recordField "revision" ( ( \( _, a, _ ) -> fromIntegral a :: Natural ) >$< ( Dhall.inject ) )
        , recordField "age" ( ( \( _, _, a ) -> fromIntegral a :: Natural ) >$< ( Dhall.inject ) )
        ]
    )


foreignLibOption :: Dhall.InputType Cabal.ForeignLibOption
foreignLibOption =
  runUnion
    ( unionAlt "Standalone" ( \x -> case x of Cabal.ForeignLibStandalone -> Just () ) Dhall.inject
    )


foreignLibType :: Dhall.InputType Cabal.ForeignLibType
foreignLibType =
  runUnion
    ( mconcat
        [ unionAlt "Shared" ( \x -> case x of Cabal.ForeignLibNativeShared -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Static" ( \x -> case x of Cabal.ForeignLibNativeStatic -> Just () ; _ -> Nothing ) Dhall.inject
        ]
    )
