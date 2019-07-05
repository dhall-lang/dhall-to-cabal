{-# language LambdaCase #-}
{-# language NoMonomorphismRestriction #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ViewPatterns #-}

module DhallToCabal.FactorType
  ( KnownType(..)
  , factored
  , mapWithBindings
  )
  where

import Control.Monad ( guard )
import Data.Foldable ( foldl', toList )
import Data.Maybe ( fromMaybe )
import Data.Text (Text)
import Dhall.Optics ( transformOf )
import Lens.Micro ( over )

import DhallToCabal
import Dhall.Extra
  ( sortExpr )

import qualified Data.Map as UnorderedMap
import qualified Dhall
import qualified Dhall.Core as Dhall
import qualified Dhall.Core as Expr ( Expr(..), Var(..), Binding(..) )
import qualified Dhall.Map as Map
import qualified Dhall.Parser
import qualified Dhall.TypeCheck as Dhall


-- Note: this needs to be in topological order of CSEability, from
-- big to small.
data KnownType
  = Package
  | Library
  | ForeignLibrary
  | Benchmark
  | Executable
  | TestSuite
  | SetupBuildInfo
  | BuildInfo
  | Config
  | SourceRepo
  | RepoType
  | RepoKind
  | Compiler
  | OS
  | Extension
  | CompilerOptions
  | Arch
  | Language
  | License
  | BuildType
  | Dependency
  | VersionRange
  | Version
  | SPDX
  | LicenseId
  | LicenseExceptionId
  | Scope
  | Mixin
  | ModuleRenaming
  | ForeignLibOption
  | ForeignLibType
  | TestType
  | Flag
  deriving (Bounded, Enum, Eq, Ord, Read, Show)


-- | A 'Benchmark' is a proper subrecord of an 'Executable', but we
-- don't want to write 'Executable' in terms of 'Benchmark'! Hence,
-- limit which types we will do the common-field extraction for to
-- only 'BuildInfo', for the time being.
isCandidateSubrecord :: KnownType -> Bool
isCandidateSubrecord BuildInfo = True
isCandidateSubrecord _ = False


dhallType :: KnownType -> Dhall.Expr Dhall.Parser.Src a
dhallType t = fmap Dhall.absurd
  ( case t of
      Config -> configRecordType
      Library -> Dhall.expected library
      ForeignLibrary -> Dhall.expected foreignLib
      Executable -> Dhall.expected executable
      Benchmark -> Dhall.expected benchmark
      TestSuite -> Dhall.expected testSuite
      SetupBuildInfo -> Dhall.expected setupBuildInfo
      BuildInfo -> buildInfoType
      SourceRepo -> Dhall.expected sourceRepo
      RepoType -> Dhall.expected repoType
      RepoKind -> Dhall.expected repoKind
      Compiler -> Dhall.expected compilerFlavor
      OS -> Dhall.expected operatingSystem
      Extension -> Dhall.expected extension
      CompilerOptions -> Dhall.expected compilerOptions
      Arch -> Dhall.expected arch
      Language -> Dhall.expected language
      License -> Dhall.expected license
      BuildType -> Dhall.expected buildType
      Package -> Dhall.expected genericPackageDescription
      Dependency -> Dhall.expected dependency
      VersionRange -> Dhall.expected versionRange
      Version -> Dhall.expected version
      SPDX -> Dhall.expected spdxLicense
      LicenseId -> Dhall.expected spdxLicenseId
      LicenseExceptionId -> Dhall.expected spdxLicenseExceptionId
      Scope -> Dhall.expected executableScope
      Mixin -> Dhall.expected mixin
      ModuleRenaming -> Dhall.expected moduleRenaming
      ForeignLibOption -> Dhall.expected foreignLibOption
      ForeignLibType -> Dhall.expected foreignLibType
      TestType -> Dhall.expected testSuiteInterface
      Flag -> Dhall.expected flag
  )


factored :: KnownType -> Expr.Expr Dhall.Parser.Src KnownType
factored rootType =
  sortExpr ( foldl' step ( dhallType rootType ) [ minBound .. maxBound ] )
  where
    step expr factorType =
      fmap
        ( fromMaybe factorType )
        ( cse
          ( isCandidateSubrecord factorType )
          ( dhallType factorType )
          expr
        )


-- | No variables should be free in the expression to lift.
cse
  :: ( Eq s, Eq a )
  => Bool
     -- ^ Should we attempt to find the subexpression as a sub-record?
  -> Expr.Expr s a
     -- ^ The common subexpression to lift.
  -> Expr.Expr s a
     -- ^ The expression to remove a common subexpression from.
  -> Expr.Expr s ( Maybe a )
     -- ^ 'Nothing' if it's representing a lifted subexpression.
cse subrecord ( fmap Just -> body ) ( fmap Just -> expr ) =
  case transformOf Dhall.subExpressions go expr of
    -- Don't lift the whole thing out - it's not a win.
    Expr.Embed Nothing ->
      body
    expr' ->
      expr'

  where

    go e | e == body =
      Expr.Embed Nothing

    go e | subrecord, Just extra <- subtractRecordFields e body =
      Expr.CombineTypes ( Expr.Embed Nothing ) extra

    go e =
      e


subtractRecordFields
  :: ( Eq s, Eq a )
  => Expr.Expr s a
  -> Expr.Expr s a
  -> Maybe ( Expr.Expr s a )
subtractRecordFields a b = do

  Expr.Record left <-
    return a

  Expr.Record right <-
    return b

  let
    intersection =
      Map.intersectionWith (==) left right

  -- The right record cannot have any fields not in left.
  guard ( null ( Map.difference right left ) )

  -- We must have at least one field with a common name
  guard ( not ( null intersection ) )

  -- All common fields must have identical types
  guard ( and intersection )

  let
    extra =
      Map.difference left right

  guard ( not ( null extra ) )

  return ( Expr.Record extra )


chunkExprs
  :: ( Applicative f )
  => ( Expr.Expr s a -> f ( Expr.Expr t b ) )
  -> Dhall.Chunks s a -> f ( Dhall.Chunks t b )
chunkExprs f ( Dhall.Chunks chunks final ) =
  flip Dhall.Chunks final <$> traverse ( traverse f ) chunks


-- | The return value of this should be linted.
mapWithBindings
  :: ( ( Text -> Expr.Var ) -> a -> b )
  -> Expr.Expr s a
  -> Expr.Expr s b
mapWithBindings f =
  go UnorderedMap.empty

  where

    outermostVar bindings n =
      Expr.V n ( fromMaybe 0 ( UnorderedMap.lookup n bindings ) )

    shiftName =
      UnorderedMap.alter ( Just . succ . fromMaybe 0 )

    go bindings = \case
      Expr.Lam n t b ->
        Expr.Lam n
          ( go bindings t )
          ( go ( shiftName n bindings ) b )

      Expr.Pi n t b ->
        Expr.Pi n ( go bindings t ) ( go ( shiftName n bindings ) b )

      Expr.App f a ->
        Expr.App ( go bindings f ) ( go bindings a )

      Expr.Let bs e ->
        go' ( toList bs ) bindings
          where
            -- Since we lint afterwards, it's fine to transform one
            -- let with many bindings into many lets with one
            -- binding each.
            go' ( Expr.Binding n t b : bs ) bindings' =
              Expr.Let
                ( pure
                  ( Expr.Binding n
                    ( fmap ( go bindings' ) t )
                    ( go bindings' b )
                  )
                )
                ( go' bs ( shiftName n bindings' ) )
            go' [] bindings' =
              go bindings' e

      Expr.Annot a b ->
        Expr.Annot ( go bindings a ) ( go bindings b )

      Expr.BoolAnd a b ->
        Expr.BoolAnd ( go bindings a ) ( go bindings b )

      Expr.BoolOr a b ->
        Expr.BoolOr ( go bindings a ) ( go bindings b )

      Expr.BoolEQ a b ->
        Expr.BoolEQ ( go bindings a ) ( go bindings b )

      Expr.BoolNE a b ->
        Expr.BoolNE ( go bindings a ) ( go bindings b )

      Expr.BoolIf a b c ->
        Expr.BoolIf ( go bindings a ) ( go bindings b ) ( go bindings c )

      Expr.NaturalPlus a b ->
        Expr.NaturalPlus ( go bindings a ) ( go bindings b )

      Expr.NaturalTimes a b ->
        Expr.NaturalTimes ( go bindings a ) ( go bindings b )

      Expr.ListAppend a b ->
        Expr.ListAppend ( go bindings a ) ( go bindings b )

      Expr.Combine a b ->
        Expr.Combine ( go bindings a ) ( go bindings b )

      Expr.Prefer a b ->
        Expr.Prefer ( go bindings a ) ( go bindings b )

      Expr.TextAppend a b ->
        Expr.TextAppend ( go bindings a ) ( go bindings b )

      Expr.ListLit t elems ->
        Expr.ListLit
          ( fmap ( go bindings ) t )
          ( fmap ( go bindings ) elems )

      Expr.OptionalLit t elems ->
        Expr.OptionalLit
          ( go bindings t )
          ( fmap ( go bindings ) elems )

      Expr.Some a ->
        Expr.Some ( go bindings a )

      Expr.None ->
        Expr.None

      Expr.Record fields ->
        Expr.Record ( fmap ( go bindings ) fields )

      Expr.RecordLit fields ->
        Expr.RecordLit ( fmap ( go bindings ) fields )

      Expr.Union fields ->
        Expr.Union ( fmap ( fmap ( go bindings ) ) fields )

      Expr.UnionLit n a fields ->
        Expr.UnionLit n ( go bindings a ) ( fmap ( fmap ( go bindings ) ) fields )

      Expr.Merge a b t ->
        Expr.Merge ( go bindings a ) ( go bindings b ) ( fmap ( go bindings ) t )

      Expr.Field e f ->
        Expr.Field ( go bindings e ) f

      Expr.Note s e ->
        Expr.Note s ( go bindings e )

      Expr.CombineTypes a b ->
        Expr.CombineTypes ( go bindings a ) ( go bindings b )

      Expr.Project e fs ->
        Expr.Project
          ( go bindings e )
          ( go bindings <$> fs )

      Expr.ImportAlt l r ->
        Expr.ImportAlt ( go bindings l ) ( go bindings r )

      Expr.IntegerToDouble ->
        Expr.IntegerToDouble

      Expr.Embed a ->
        Expr.Embed
          ( f ( outermostVar bindings ) a )

      Expr.Const c ->
        Expr.Const c

      Expr.Var v ->
        Expr.Var v

      Expr.Bool ->
        Expr.Bool

      Expr.BoolLit b ->
        Expr.BoolLit b

      Expr.Natural ->
        Expr.Natural

      Expr.NaturalLit n ->
        Expr.NaturalLit n

      Expr.NaturalFold ->
        Expr.NaturalFold

      Expr.NaturalBuild ->
        Expr.NaturalBuild

      Expr.NaturalIsZero ->
        Expr.NaturalIsZero

      Expr.NaturalEven ->
        Expr.NaturalEven

      Expr.NaturalOdd ->
        Expr.NaturalOdd

      Expr.NaturalToInteger ->
        Expr.NaturalToInteger

      Expr.NaturalShow ->
        Expr.NaturalShow

      Expr.Integer ->
        Expr.Integer

      Expr.IntegerShow ->
        Expr.IntegerShow

      Expr.IntegerLit n ->
        Expr.IntegerLit n

      Expr.Double ->
        Expr.Double

      Expr.DoubleShow ->
        Expr.DoubleShow

      Expr.DoubleLit n ->
        Expr.DoubleLit n

      Expr.Text ->
        Expr.Text

      Expr.TextLit t ->
        Expr.TextLit ( over chunkExprs ( go bindings ) t )

      Expr.TextShow ->
        Expr.TextShow

      Expr.List ->
        Expr.List

      Expr.ListBuild ->
        Expr.ListBuild

      Expr.ListFold ->
        Expr.ListFold

      Expr.ListLength ->
        Expr.ListLength

      Expr.ListHead ->
        Expr.ListHead

      Expr.ListLast ->
        Expr.ListLast

      Expr.ListIndexed ->
        Expr.ListIndexed

      Expr.ListReverse ->
        Expr.ListReverse

      Expr.Optional ->
        Expr.Optional

      Expr.OptionalFold ->
        Expr.OptionalFold

      Expr.OptionalBuild ->
        Expr.OptionalBuild
