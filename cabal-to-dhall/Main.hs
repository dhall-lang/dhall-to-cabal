{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

module Main ( main ) where

import Control.Applicative ( (<**>), optional )
import Data.Foldable ( foldMap )
import Data.Functor.Contravariant ( (>$<), Contravariant( contramap ) )
import Data.Maybe ( listToMaybe )
import Data.Monoid ( First(..), (<>) )
import GHC.Stack

import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Core as Expr ( Const(..), Expr(..) )
import qualified Dhall.Parser
import qualified Dhall.TypeCheck
import qualified Distribution.Compiler as Cabal
import qualified Distribution.License as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.System as Cabal
import qualified Distribution.Text as Cabal
import qualified Distribution.Types.BuildInfo as Cabal
import qualified Distribution.Types.BuildType as Cabal
import qualified Distribution.Types.CondTree as Cabal
import qualified Distribution.Types.Condition as Cabal
import qualified Distribution.Types.Dependency as Cabal
import qualified Distribution.Types.ExeDependency as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal
import qualified Distribution.Types.LegacyExeDependency as Cabal
import qualified Distribution.Types.Library as Cabal
import qualified Distribution.Types.ModuleReexport as Cabal
import qualified Distribution.Types.PackageDescription as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PkgconfigDependency as Cabal
import qualified Distribution.Types.PkgconfigName as Cabal
import qualified Distribution.Types.SetupBuildInfo as Cabal
import qualified Distribution.Types.SourceRepo as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Version as Cabal
import qualified Options.Applicative as OptParse

import qualified Distribution.Package.Dhall as DhallToCabal


type DhallExpr =
  Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X


data Command
  = RunCabalToDhall CabalToDhallOptions


data CabalToDhallOptions = CabalToDhallOptions
  { cabalFilePath :: Maybe String
  }


cabalToDhallOptionsParser :: OptParse.Parser CabalToDhallOptions
cabalToDhallOptionsParser =
  CabalToDhallOptions
    <$>
      optional
        ( OptParse.argument
            OptParse.str
            ( mconcat
                [ OptParse.metavar "<cabal input file>"
                , OptParse.help "The Cabal file to convert to Dhall"
                ]
            )
        )


commandLineParser =
  RunCabalToDhall <$> ( cabalToDhallOptionsParser <**> OptParse.helper )


main :: IO ()
main = do
  command <-
    OptParse.execParser
      ( OptParse.info commandLineParser mempty )

  case command of
    RunCabalToDhall options ->
      runCabalToDhall options


runCabalToDhall :: CabalToDhallOptions -> IO ()
runCabalToDhall CabalToDhallOptions{ cabalFilePath } = do
  source <-
    case cabalFilePath of
      Nothing ->
        LazyText.getContents

      Just filePath ->
        LazyText.readFile filePath

  case Cabal.parseGenericPackageDescription ( LazyText.unpack source ) of
    Cabal.ParseFailed e -> do
      putStrLn "Could not parse Cabal file: "

      error ( show e )

    Cabal.ParseOk _warnings genericPackageDescription -> do
      let
        dhall =
          Dhall.embed genericPackageDescriptionToDhall genericPackageDescription

      LazyText.putStrLn ( Dhall.Core.pretty dhall )


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
        \a -> Expr.RecordLit ( fmap ( \t -> Dhall.embed t a ) m )
    , Dhall.declared = Expr.Record ( fmap Dhall.declared m )
    }


genericPackageDescriptionToDhall
  :: Dhall.InputType Cabal.GenericPackageDescription
genericPackageDescriptionToDhall =
  runRecordInputType
    ( mconcat
        [ Cabal.packageDescription >$< packageDescriptionToRecord
        , recordField "flags" ( Cabal.genPackageFlags >$< ( listOf flag ) )
        , recordField "library" ( Cabal.condLibrary >$< maybeToDhall ( condTree library ) )
        ]
    )


packageDescriptionToRecord
  :: RecordInputType Cabal.PackageDescription
packageDescriptionToRecord =
  mconcat
    [ contramap Cabal.package packageIdentifierToRecord
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
    , recordField "source-repos" ( contramap Cabal.sourceRepos ( listOf sourceRepo ) )
    , recordField "synopsis" ( contramap Cabal.synopsis stringToDhall )
    , recordField "description" ( contramap Cabal.description stringToDhall )
    , recordField "category" ( contramap Cabal.category stringToDhall )
    , recordField "cabal-version" ( contramap Cabal.specVersionRaw specVersion )
    , recordField "build-type" ( contramap Cabal.buildType ( maybeToDhall buildType ) )
    , recordField "data-files" ( contramap Cabal.dataFiles ( listOf stringToDhall ) )
    , recordField "data-dir" ( contramap Cabal.dataDir stringToDhall )
    , recordField "extra-src-files" ( contramap Cabal.extraSrcFiles ( listOf stringToDhall ) )
    , recordField "extra-doc-files" ( contramap Cabal.extraDocFiles ( listOf stringToDhall ) )
    , recordField "extra-tmp-files" ( contramap Cabal.extraTmpFiles ( listOf stringToDhall ) )
    , recordField "custom-setup" ( contramap Cabal.setupBuildInfo ( maybeToDhall setupBuildInfo ) )
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
        \a ->
          Expr.Lam "Version" ( Expr.Const Expr.Type )
            $ Expr.Lam "v" ( Expr.Pi "_" Expr.Text ( Expr.Var "Version" ) )
            $ Expr.App
                ( Expr.Var "v" )
                ( Dhall.embed stringToDhall ( show ( Cabal.disp a ) ) )
    , Dhall.declared =
        Dhall.expected DhallToCabal.version
    }


stringToDhall :: Dhall.InputType String
stringToDhall =
  contramap LazyText.pack Dhall.inject


licenseToDhall :: Dhall.InputType Cabal.License
licenseToDhall =
  runUnion
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
        , unknown
        ]
    )

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
      unionAlt "Unspecified" ( \l -> case l of Cabal.UnspecifiedLicense -> Just () ; _ -> Nothing ) Dhall.inject

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


runUnion :: HasCallStack => Union a -> Dhall.InputType a
runUnion ( Union ( f, t ) ) =
  Dhall.InputType
    { Dhall.embed =
        \a ->
          case f a of
            ( First Nothing, _ ) ->
              error "Union did not match anything"

            ( First ( Just ( k, v ) ), alts ) ->
              Expr.UnionLit k v alts
    , Dhall.declared =
        Expr.Union t
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
        \a -> Expr.OptionalLit ( Dhall.declared t ) ( foldMap pure ( Dhall.embed t <$> a ) )
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
  Dhall.inject


versionRange :: Dhall.InputType Cabal.VersionRange
versionRange =
  Dhall.InputType
    { Dhall.embed =
        \versionRange0 ->
          Expr.Lam "VersionRange" ( Expr.Const Expr.Type )
            $ Expr.Lam "anyVersion" ( Expr.Var "VersionRange" )
            $ Expr.Lam "noVersion" ( Expr.Var "VersionRange" )
            $ Expr.Lam "thisVersion" ( Expr.Pi "_" ( Dhall.expected DhallToCabal.version ) ( Expr.Var "VersionRange" ) )
            $ Expr.Lam "notThisVersion" ( Expr.Pi "_" ( Dhall.expected DhallToCabal.version ) ( Expr.Var "VersionRange" ) )
            $ Expr.Lam "laterVersion" ( Expr.Pi "_" ( Dhall.expected DhallToCabal.version ) ( Expr.Var "VersionRange" ) )
            $ Expr.Lam "earlierVersion" ( Expr.Pi "_" ( Dhall.expected DhallToCabal.version ) ( Expr.Var "VersionRange" ) )
            $ Expr.Lam "orLaterVersion" ( Expr.Pi "_" ( Dhall.expected DhallToCabal.version ) ( Expr.Var "VersionRange" ) )
            $ Expr.Lam "orEarlierVersion" ( Expr.Pi "_" ( Dhall.expected DhallToCabal.version ) ( Expr.Var "VersionRange" ) )
            $ Expr.Lam "withinVersion" ( Expr.Pi "_" ( Dhall.expected DhallToCabal.version ) ( Expr.Var "VersionRange" ) )
            $ Expr.Lam "majorBoundVersion" ( Expr.Pi "_" ( Dhall.expected DhallToCabal.version ) ( Expr.Var "VersionRange" ) )
            $ Expr.Lam "unionVersionRanges" ( Expr.Pi "_" ( Expr.Var "VersionRange" ) ( Expr.Pi "_" ( Expr.Var "VersionRange" ) ( Expr.Var "VersionRange" ) ) )
            $ Expr.Lam "intersectVersionRanges" ( Expr.Pi "_" ( Expr.Var "VersionRange" ) ( Expr.Pi "_" ( Expr.Var "VersionRange" ) ( Expr.Var "VersionRange" ) ) )
            $ Expr.Lam "differenceVersionRanges" ( Expr.Pi "_" ( Expr.Var "VersionRange" ) ( Expr.Pi "_" ( Expr.Var "VersionRange" ) ( Expr.Var "VersionRange" ) ) )
            $ Expr.Lam "invertVersionRange" ( Expr.Pi "_" ( Expr.Var "VersionRange" ) ( Expr.Var "VersionRange" ) )
            $ let
                go versionRange =
                  case versionRange of
                    Cabal.AnyVersion ->
                      Expr.Var "anyVersion"

                    Cabal.IntersectVersionRanges ( Cabal.LaterVersion ( Cabal.versionNumbers -> [ 1 ] ) ) ( Cabal.EarlierVersion ( Cabal.versionNumbers -> [ 1 ] ) ) ->
                      Expr.Var "noVersion"

                    Cabal.ThisVersion v ->
                      Expr.App ( Expr.Var "thisVersion" ) ( Dhall.embed versionToDhall v )

                    Cabal.UnionVersionRanges ( Cabal.EarlierVersion v ) ( Cabal.LaterVersion v' ) | v == v' ->
                      Expr.App ( Expr.Var "notThisVersion" ) ( Dhall.embed versionToDhall v )

                    Cabal.LaterVersion v ->
                      Expr.App ( Expr.Var "laterVersion" ) ( Dhall.embed versionToDhall v )

                    Cabal.UnionVersionRanges ( Cabal.ThisVersion v ) ( Cabal.LaterVersion v' ) | v == v' ->
                      Expr.App ( Expr.Var "orLaterVersion" ) ( Dhall.embed versionToDhall v )

                    Cabal.EarlierVersion v ->
                      Expr.App ( Expr.Var "earlierVersion" ) ( Dhall.embed versionToDhall v )

                    Cabal.UnionVersionRanges ( Cabal.ThisVersion v ) ( Cabal.EarlierVersion v' ) | v == v' ->
                      Expr.App ( Expr.Var "orEarlierVersion" ) ( Dhall.embed versionToDhall v )

                    Cabal.UnionVersionRanges a b ->
                      Expr.App ( Expr.App ( Expr.Var "unionVersionRanges" ) ( go a ) ) ( go b )

                    Cabal.IntersectVersionRanges a b ->
                      Expr.App ( Expr.App ( Expr.Var "intersectVersionRanges" ) ( go a ) ) ( go b )

                    Cabal.WildcardVersion v ->
                      Expr.App ( Expr.Var "withinVersion" ) ( Dhall.embed versionToDhall v )

                    Cabal.MajorBoundVersion v ->
                      Expr.App ( Expr.Var "majorBoundVersion" ) ( Dhall.embed versionToDhall v )

                    Cabal.VersionRangeParens vr ->
                      go vr

              in
                go ( Cabal.fromVersionIntervals ( Cabal.toVersionIntervals versionRange0 ) )
    , Dhall.declared = Dhall.expected DhallToCabal.versionRange
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
  runUnion
    ( mconcat
        [ unionAlt "RepoHead" ( \x -> case x of Cabal.RepoHead -> Just () ; _ -> Nothing) Dhall.inject
        , unionAlt "RepoThis" ( \x -> case x of Cabal.RepoThis -> Just () ; _ -> Nothing) Dhall.inject
        , unionAlt "RepoKindUnknown" ( \x -> case x of Cabal.RepoKindUnknown str -> Just  str ; _ -> Nothing) stringToDhall
        ]
    )


repoType :: Dhall.InputType Cabal.RepoType
repoType =
  runUnion
    ( mconcat
        [ unionAlt "Darcs" ( \x -> case x of Cabal.Darcs -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Git" ( \x -> case x of Cabal.Git -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "SVN" ( \x -> case x of Cabal.SVN -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "CVS" ( \x -> case x of Cabal.CVS -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Mercurial" ( \x -> case x of Cabal.Mercurial -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "GnuArch" ( \x -> case x of Cabal.GnuArch -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Bazaar" ( \x -> case x of Cabal.Bazaar -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Monotone" ( \x -> case x of Cabal.Monotone -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "OtherRepoType" ( \x -> case x of Cabal.OtherRepoType s -> Just s ; _ -> Nothing ) stringToDhall
        ]
    )


specVersion :: Dhall.InputType ( Either Cabal.Version Cabal.VersionRange )
specVersion =
  Dhall.InputType
    { Dhall.embed = either ( Dhall.embed versionToDhall ) ( error "Only exact cabal-versions are supported" )
    , Dhall.declared = Dhall.declared versionToDhall
    }


buildType :: Dhall.InputType Cabal.BuildType
buildType =
  runUnion
    ( mconcat
        [ unionAlt "Simple" ( \x -> case x of Cabal.Simple -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Configure" ( \x -> case x of Cabal.Configure -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Make" ( \x -> case x of Cabal.Make -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Custom" ( \x -> case x of Cabal.Custom -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "UnknownBuildType" ( \x -> case x of Cabal.UnknownBuildType s -> Just s ; _ -> Nothing ) stringToDhall
        ]
    )


setupBuildInfo :: Dhall.InputType Cabal.SetupBuildInfo
setupBuildInfo =
  runRecordInputType
    ( mconcat
        [ recordField "setup-depends" ( contramap Cabal.setupDepends ( listOf dependency ) )
        ]
    )


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
  runRecordInputType
    ( mconcat
        [ recordField
            "exposed-modules"
            ( contramap Cabal.exposedModules ( listOf moduleName ) )
        , recordField
            "reexported-modules"
            ( contramap Cabal.reexportedModules ( listOf moduleReexport ) )
        , recordField
            "signatures"
            ( contramap Cabal.signatures ( listOf moduleName ) )
        , contramap Cabal.libBuildInfo buildInfoRecord
        ]
    )


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

  in
  Dhall.InputType
    { Dhall.embed = Expr.Lam "config" DhallToCabal.configRecordType . go . unifyCondTree
    , Dhall.declared = Expr.Pi "_" DhallToCabal.configRecordType ( Dhall.declared t )
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
  runUnion
    ( mconcat
        [ unionAlt "Linux" ( \x -> case x of Cabal.Linux -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Windows" ( \x -> case x of Cabal.Windows -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "OSX" ( \x -> case x of Cabal.OSX -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "FreeBSD" ( \x -> case x of Cabal.FreeBSD -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "OpenBSD" ( \x -> case x of Cabal.OpenBSD -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "NetBSD" ( \x -> case x of Cabal.NetBSD -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "DragonFly" ( \x -> case x of Cabal.DragonFly -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Solaris" ( \x -> case x of Cabal.Solaris -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "AIX" ( \x -> case x of Cabal.AIX -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "HPUX" ( \x -> case x of Cabal.HPUX -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "IRIX" ( \x -> case x of Cabal.IRIX -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "HaLVM" ( \x -> case x of Cabal.HaLVM -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Hurd" ( \x -> case x of Cabal.Hurd -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "IOS" ( \x -> case x of Cabal.IOS -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Android" ( \x -> case x of Cabal.Android -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "Ghcjs" ( \x -> case x of Cabal.Ghcjs -> Just () ; _ -> Nothing ) Dhall.inject
        , unionAlt "OtherOS" ( \x -> case x of Cabal.OtherOS s -> Just s ; _ -> Nothing ) ( runRecordInputType ( recordField "_1" stringToDhall ) )
        ]
    )


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
    , recordField "autegen-modules" ( contramap Cabal.autogenModules ( listOf moduleName ) ) 
    -- , recordField "default-language" ( contramap Cabal.defaultLanguage ( maybeToDhall language ) )
    -- , recordField "other-languages" ( contramap Cabal.otherLanguages ( listOf language ) )
    -- , recordField "default-extensions" ( Cabal.defaultExtensions >$< listOf extension )
    -- , recordField "other-extensions" ( Cabal.otherExtensions >$< listOf extension )
    , recordField "extra-libraries" ( Cabal.extraLibs >$< listOf stringToDhall )
    , recordField "extra-ghci-libraries" ( Cabal.extraGHCiLibs >$< listOf stringToDhall )
    , recordField "extra-lib-dirs" ( Cabal.extraLibDirs >$< listOf stringToDhall )
    , recordField "include-dirs" ( Cabal.includeDirs >$< listOf stringToDhall )
    , recordField "includes" ( Cabal.includes >$< listOf stringToDhall )
    , recordField "install-includes" ( Cabal.installIncludes >$< listOf stringToDhall )
    -- , recordField "compiler-options" ( Cabal.options >$< compilerOptions )
    -- , recordField "profiling-options" ( Cabal.profOptions >$< compilerOptions )
    -- , recordField "shared-options" ( Cabal.sharedOptions >$< compilerOptions )
    , recordField "build-depends" ( Cabal.targetBuildDepends >$< listOf dependency )
    -- , recordField "mixins" ( Cabal.mixins >$< listOf mixin )
    ]


moduleReexport :: Dhall.InputType Cabal.ModuleReexport
moduleReexport =
  runRecordInputType
    mempty


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
