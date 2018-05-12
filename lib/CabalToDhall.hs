{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

module CabalToDhall ( cabalToDhall ) where

import Control.Monad ( join )
import Data.Foldable ( foldMap )
import Data.Functor.Contravariant ( (>$<), Contravariant( contramap ) )
import Data.Monoid ( First(..), (<>) )
import GHC.Stack
import Numeric.Natural ( Natural )

import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Data.Text.Lazy as LazyText
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Distribution.Compiler as Cabal
import qualified Distribution.License as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
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

import DhallToCabal ( sortExpr )


preludeLocation :: Dhall.Core.Path
preludeLocation =
  Dhall.Core.Path
    { Dhall.Core.pathHashed =
        Dhall.Core.PathHashed
          { Dhall.Core.hash =
              Nothing
          , Dhall.Core.pathType =
              Dhall.Core.URL
                "https://raw.githubusercontent.com/dhall-lang/dhall-to-cabal/1.0.0/dhall/prelude.dhall"
                Nothing
          }
    , Dhall.Core.pathMode =
        Dhall.Core.Code
    }


typesLocation :: Dhall.Core.Path
typesLocation =
  Dhall.Core.Path
    { Dhall.Core.pathHashed =
        Dhall.Core.PathHashed
          { Dhall.Core.hash =
              Nothing
          , Dhall.Core.pathType =
              Dhall.Core.URL
                "https://raw.githubusercontent.com/dhall-lang/dhall-to-cabal/1.0.0/dhall/types.dhall"
                Nothing
          }
    , Dhall.Core.pathMode =
        Dhall.Core.Code
    }


type DhallExpr =
  Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X


cabalToDhall :: LazyText.Text -> IO LazyText.Text
cabalToDhall source =
  case Cabal.parseGenericPackageDescription ( LazyText.unpack source ) of
    Cabal.ParseFailed e -> do
      putStrLn "Could not parse Cabal file: "

      error ( show e )

    Cabal.ParseOk _warnings genericPackageDescription -> do
      let
        dhall =
          Expr.Let "prelude" Nothing ( Expr.Embed preludeLocation )
            $ Expr.Let "types" Nothing ( Expr.Embed typesLocation )
            $ ( Dhall.TypeCheck.absurd <$>
                Dhall.embed
                  genericPackageDescriptionToDhall
                  genericPackageDescription
              )

      return ( Dhall.Core.pretty dhall )


newtype RecordInputType a =
  RecordInputType
    { unRecordInputType ::
        Map.InsOrdHashMap Dhall.Text ( Dhall.InputType a )
    }
  deriving ( Monoid )


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
  runRecordInputType
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
    , recordField "build-type" ( contramap Cabal.buildType ( maybeToDhall buildType ) )
    , recordField "license" ( contramap Cabal.license licenseToDhall )
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
  contramap LazyText.pack Dhall.inject


licenseToDhall :: Dhall.InputType Cabal.License
licenseToDhall =
  ( runUnion
      ( mconcat
          [ gpl
          , agpl
          , lgpl
          , bsd2
          , bsd3
          , bsd4
          , mit
          , isc
          , mpl
          , apache
          , publicDomain
          , allRightsReserved
          , unspecified
          , other
          -- , unknown
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` "License"
    }

  where

    gpl =
      unionAlt "GPL" ( \l -> case l of Cabal.GPL v -> Just v; _ -> Nothing ) ( maybeToDhall versionToDhall )

    agpl =
      unionAlt "AGPL" ( \l -> case l of Cabal.AGPL v -> Just v ; _ -> Nothing ) ( maybeToDhall versionToDhall )

    lgpl =
      unionAlt "LGPL" ( \l -> case l of Cabal.LGPL v -> Just v ; _ -> Nothing ) ( maybeToDhall versionToDhall )

    bsd2 =
      unionAlt "BSD2" ( \l -> case l of Cabal.BSD2 -> Just () ; _ -> Nothing ) Dhall.inject

    bsd3 =
      unionAlt "BSD3" ( \l -> case l of Cabal.BSD3 -> Just () ; _ -> Nothing ) Dhall.inject

    bsd4 =
      unionAlt "BSD4" ( \l -> case l of Cabal.BSD4 -> Just () ; _ -> Nothing ) Dhall.inject

    mit =
      unionAlt "MIT" ( \l -> case l of Cabal.MIT -> Just () ; _ -> Nothing ) Dhall.inject

    isc =
      unionAlt "ISC" ( \l -> case l of Cabal.ISC -> Just () ; _ -> Nothing ) Dhall.inject

    mpl =
      unionAlt "MPL" ( \l -> case l of Cabal.MPL v -> Just v ; _ -> Nothing ) versionToDhall

    apache =
      unionAlt "Apache" ( \l -> case l of Cabal.Apache v -> Just v ; _ -> Nothing ) ( maybeToDhall versionToDhall )

    publicDomain =
      unionAlt "PublicDomain" ( \l -> case l of Cabal.PublicDomain -> Just () ; _ -> Nothing ) Dhall.inject

    allRightsReserved =
      unionAlt "AllRightsReserved" ( \l -> case l of Cabal.AllRightsReserved -> Just () ; _ -> Nothing ) Dhall.inject

    unspecified =
      unionAlt
        "Unspecified"
        ( \l ->
            case l of
              Cabal.UnspecifiedLicense ->
                Just ()

              Cabal.UnknownLicense "UnspecifiedLicense" ->
                Just ()

              _ ->
                Nothing
        )
        Dhall.inject

    other =
      unionAlt "Other" ( \l -> case l of Cabal.OtherLicense -> Just () ; _ -> Nothing ) Dhall.inject

    unknown =
      unionAlt "Unknown" ( \l -> case l of Cabal.UnknownLicense s -> Just s ; _ -> Nothing ) stringToDhall


newtype Union a =
  Union
    { unUnion ::
        ( a ->
          ( First ( Dhall.Text, DhallExpr )
          , Map.InsOrdHashMap Dhall.Text DhallExpr
          )
        , Map.InsOrdHashMap Dhall.Text DhallExpr
        )
    }
  deriving ( Monoid )


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

          Just b ->
            ( First ( fmap ( \b -> ( k, Dhall.embed t b ) ) ( f a ) ), mempty )
    , Map.singleton k ( Dhall.declared t )
    )


maybeToDhall :: Dhall.InputType a -> Dhall.InputType ( Maybe a )
maybeToDhall t =
  Dhall.InputType
    { Dhall.embed =
        \a -> Expr.OptionalLit ( Dhall.declared t ) ( Dhall.embed t <$> a )
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


instance {-# OVERLAPS #-} Dhall.Inject [Char] where
  injectWith _ = stringToDhall

instance Dhall.Inject Cabal.CompilerFlavor


compilerFlavor :: Dhall.InputType Cabal.CompilerFlavor
compilerFlavor =
  let
    constructor k v =
      Expr.App ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "Compilers" `Expr.Field` k ) v

  in
  Dhall.InputType
    { Dhall.embed = \case
        Cabal.GHC ->
          constructor "GHC" ( Expr.RecordLit mempty )

        Cabal.GHCJS ->
          constructor "GHCJS" ( Expr.RecordLit mempty )

        Cabal.HBC ->
          constructor "HBC" ( Expr.RecordLit mempty )

        Cabal.HaskellSuite v ->
          constructor "HaskellSuite" ( Expr.Record ( Map.singleton "_1" ( Dhall.embed Dhall.inject v ) ) )

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
          constructor "OtherCompiler" ( Expr.Record ( Map.singleton "_1" ( Dhall.embed Dhall.inject v ) ) )

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
            go versionRange =
              case versionRange of
                Cabal.AnyVersion ->
                  Expr.Var "prelude" `Expr.Field` "anyVersion"

                Cabal.IntersectVersionRanges ( Cabal.LaterVersion ( Cabal.versionNumbers -> [ 1 ] ) ) ( Cabal.EarlierVersion ( Cabal.versionNumbers -> [ 1 ] ) ) ->
                  Expr.Var "prelude" `Expr.Field` "noVersion"

                Cabal.ThisVersion v ->
                  Expr.App
                    ( Expr.Var "prelude" `Expr.Field` "thisVersion" )
                    ( Dhall.embed versionToDhall v )

                Cabal.UnionVersionRanges ( Cabal.EarlierVersion v ) ( Cabal.LaterVersion v' ) | v == v' ->
                  Expr.App
                    ( Expr.Var "prelude" `Expr.Field` "notThisVersion" )
                    ( Dhall.embed versionToDhall v )

                Cabal.LaterVersion v ->
                  Expr.App
                    ( Expr.Var "prelude" `Expr.Field` "laterVersion" )
                    ( Dhall.embed versionToDhall v )

                Cabal.UnionVersionRanges ( Cabal.ThisVersion v ) ( Cabal.LaterVersion v' ) | v == v' ->
                  Expr.App
                    ( Expr.Var "prelude" `Expr.Field` "orLaterVersion" )
                    ( Dhall.embed versionToDhall v )

                Cabal.EarlierVersion v ->
                  Expr.App
                    ( Expr.Var "prelude" `Expr.Field` "earlierVersion" )
                    ( Dhall.embed versionToDhall v )

                Cabal.UnionVersionRanges ( Cabal.ThisVersion v ) ( Cabal.EarlierVersion v' ) | v == v' ->
                  Expr.App
                    ( Expr.Var "prelude" `Expr.Field` "orEarlierVersion" )
                    ( Dhall.embed versionToDhall v )

                Cabal.UnionVersionRanges a b ->
                  Expr.App
                    ( Expr.App ( Expr.Var "prelude" `Expr.Field` "unionVersionRanges" ) ( go a ) )
                    ( go b )

                Cabal.IntersectVersionRanges a b ->
                  Expr.App
                    ( Expr.App ( Expr.Var "prelude" `Expr.Field` "intersectVersionRanges" ) ( go a ) )
                    ( go b )

                Cabal.WildcardVersion v ->
                  Expr.App
                    ( Expr.Var "prelude" `Expr.Field` "withinVersion" )
                    ( Dhall.embed versionToDhall v )

                Cabal.MajorBoundVersion v ->
                  Expr.App
                    ( Expr.Var "prelude" `Expr.Field` "majorBoundVersion" )
                    ( Dhall.embed versionToDhall v )

                Cabal.VersionRangeParens vr ->
                  go vr

          in
          go ( Cabal.fromVersionIntervals ( Cabal.toVersionIntervals versionRange0 ) )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "VersionRange"
    }


sourceRepo :: Dhall.InputType Cabal.SourceRepo
sourceRepo =
  runRecordInputType
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


repoKind :: Dhall.InputType Cabal.RepoKind
repoKind =
  ( runUnion
      ( mconcat
          [ unionAlt "RepoThis" ( \x -> case x of Cabal.RepoThis -> Just () ; _ -> Nothing) Dhall.inject
          , unionAlt "RepoKindUnknown" ( \x -> case x of Cabal.RepoKindUnknown str -> Just  str ; _ -> Nothing) ( runRecordInputType ( recordField "_1" stringToDhall ) )
          , unionAlt "RepoHead" ( \x -> case x of Cabal.RepoHead -> Just () ; _ -> Nothing) Dhall.inject
          ]
      )
  )
    { Dhall.declared =
         Expr.Var "types" `Expr.Field` "RepoKind"
    }


repoType :: Dhall.InputType Cabal.RepoType
repoType =
  ( runUnion
      ( mconcat
          [ unionAlt "Darcs" ( \x -> case x of Cabal.Darcs -> Just () ; _ -> Nothing ) Dhall.inject
          , unionAlt "Git" ( \x -> case x of Cabal.Git -> Just () ; _ -> Nothing ) Dhall.inject
          , unionAlt "SVN" ( \x -> case x of Cabal.SVN -> Just () ; _ -> Nothing ) Dhall.inject
          , unionAlt "CVS" ( \x -> case x of Cabal.CVS -> Just () ; _ -> Nothing ) Dhall.inject
          , unionAlt "Mercurial" ( \x -> case x of Cabal.Mercurial -> Just () ; _ -> Nothing ) Dhall.inject
          , unionAlt "GnuArch" ( \x -> case x of Cabal.GnuArch -> Just () ; _ -> Nothing ) Dhall.inject
          , unionAlt "Monotone" ( \x -> case x of Cabal.Monotone -> Just () ; _ -> Nothing ) Dhall.inject
          , unionAlt "OtherRepoType" ( \x -> case x of Cabal.OtherRepoType s -> Just s ; _ -> Nothing ) ( runRecordInputType ( recordField "_1" stringToDhall ) )
          , unionAlt "Bazaar" ( \x -> case x of Cabal.Bazaar -> Just () ; _ -> Nothing ) Dhall.inject
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` "RepoType"
    }


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

        Cabal.UnknownBuildType s ->
          Expr.App
            ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "BuildTypes" `Expr.Field` "UnknownBuildType" )
            ( Expr.RecordLit ( Map.singleton "_1" ( Dhall.embed Dhall.inject s ) ) )

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
  ( runRecordInputType
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


data CondIfTree v a
  = Val a
  | If v ( CondIfTree v a ) ( CondIfTree v a )
  deriving (Eq, Show)


unifyCondTree
  :: ( Monoid a, Monoid x )
  => Cabal.CondTree v x a
  -> CondIfTree ( Cabal.Condition v ) a
unifyCondTree =
  let
    go acc condTree =
      case Cabal.condTreeComponents condTree of
        [] ->
          Val ( acc <> Cabal.condTreeData condTree )

        [c] ->
          If
            ( Cabal.condBranchCondition c )
            ( go
                ( acc <> Cabal.condTreeData condTree )
                ( Cabal.condBranchIfTrue c )
            )
            ( go
                ( acc <> Cabal.condTreeData condTree )
                ( case Cabal.condBranchIfFalse c of
                    Nothing ->
                      Cabal.CondNode mempty mempty mempty

                    Just c ->
                      c
                )
            )

        (c:cs) ->
          go acc ( condTree { Cabal.condTreeComponents = pushDownBranch c <$> cs } )

    pushDownBranch a b =
      b
        { Cabal.condBranchIfTrue =
            pushDownTree a ( Cabal.condBranchIfTrue b )
        , Cabal.condBranchIfFalse =
            case Cabal.condBranchIfFalse b of
              Nothing ->
                Just ( Cabal.CondNode mempty mempty [a] )

              Just tree ->
                Just ( pushDownTree a tree )
        }

    pushDownTree a b =
      b { Cabal.condTreeComponents = a : Cabal.condTreeComponents b }

  in
  go mempty


condTree
  :: ( Monoid a, Monoid x )
  => Dhall.InputType a
  -> Dhall.InputType ( Cabal.CondTree Cabal.ConfVar x a )
condTree t =
  let
    go = \case
      Val a ->
        Dhall.embed t a

      If cond a b ->
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
    , recordField "build-depends" ( Cabal.targetBuildDepends >$< listOf dependency )
    , recordField "mixins" ( Cabal.mixins >$< listOf mixin )
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

  extName :: Cabal.KnownExtension -> LazyText.Text
  extName e =
    LazyText.pack ( show e )

  extWith trueFalse ext =
    Expr.App
      ( Expr.Var "prelude" `Expr.Field` "types" `Expr.Field` "Extensions" `Expr.Field` extName ext )
      ( Expr.BoolLit trueFalse )


compilerOptions :: Dhall.InputType [ ( Cabal.CompilerFlavor, [ String ] ) ]
compilerOptions =
  Dhall.InputType
    { Dhall.embed = \l ->
        case filter ( not . null . snd ) l of
          [] ->
            Expr.Var "prelude" `Expr.Field` "defaults" `Expr.Field` "CompilerOptions"

          xs ->
            Expr.Prefer
              ( Expr.Var "prelude" `Expr.Field` "defaults" `Expr.Field` "compiler-options" )
              ( Expr.RecordLit
                  ( Map.fromList
                      ( map
                          ( \( c, opts ) ->
                              ( LazyText.pack ( show c ), Dhall.embed Dhall.inject opts )
                          )
                          xs
                      )
                  )
              )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` "CompilerOptions"
    }

  where

    field c =
      recordField ( LazyText.pack ( show c ) ) ( filtering c )

    filtering c =
      contramap
        ( \l -> join [ opts | ( c', opts ) <- l, c == c' ] )
        ( listOf stringToDhall )


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
  ( \( Cabal.ModuleRenaming a ) -> a ) >$<
  listOf
    ( runRecordInputType
        ( mconcat
            [ recordField "rename" ( ( \( a, _ ) -> a ) >$< moduleName )
            , recordField "to" ( ( \( _, a ) -> a ) >$< moduleName )
            ]
        )
    )


benchmark :: Dhall.InputType Cabal.Benchmark
benchmark =
  (  runRecordInputType
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
  ( runRecordInputType
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
  ( runRecordInputType
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
  runUnion
    ( mconcat
        [ unionAlt
            "Public"
            ( \x ->
                case x of
                  Cabal.ExecutablePublic -> Just ()
                  Cabal.ExecutableScopeUnknown -> Just ()
                  _ -> Nothing
            )
            Dhall.inject
        , unionAlt "Private" ( \x -> case x of Cabal.ExecutablePrivate -> Just () ; _ -> Nothing ) Dhall.inject
        ]
    )


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
    ( unionAlt "Standalone" ( \x -> case x of Cabal.ForeignLibStandalone -> Just () ; _ -> Nothing ) Dhall.inject
    )


foreignLibType :: Dhall.InputType Cabal.ForeignLibType
foreignLibType =
  runUnion
    ( mconcat
        [ unionAlt "Shared" ( \x -> case x of Cabal.ForeignLibNativeShared -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Static" ( \x -> case x of Cabal.ForeignLibNativeStatic -> Just () ; _ -> Nothing ) Dhall.inject
        ]
    )
