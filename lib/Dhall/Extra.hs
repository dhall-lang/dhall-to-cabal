{-# language ApplicativeDo #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module Dhall.Extra
  ( RecordBuilder
  , emptyRecord
  , keyValue
  , list
  , makeRecord
  , makeUnion
  , pair
  , string
  , validateType
  )
  where

import Control.Applicative ( Const(..) )
import Control.Exception ( Exception, throwIO )
import Control.Monad ( (>=>), guard, join )
import Control.Monad.Trans.Reader ( Reader, reader, runReader )
import Data.Foldable ( toList )
import Data.Function ( (&) )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Product ( Product(..) )
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
import qualified Distribution.Types.ExecutableScope as Cabal
import qualified Distribution.Types.ForeignLib as Cabal
import qualified Distribution.Types.ForeignLibType as Cabal
import qualified Distribution.Types.LegacyExeDependency as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Version as Cabal

import qualified Dhall.Core as Expr
  ( Const(..), Expr(..), Normalizer, Var(..) )



string :: Dhall.Type String
string =
  LazyText.unpack <$> Dhall.lazyText



exprToString :: Dhall.Expr a b -> Maybe String
exprToString expr = do
  Expr.TextLit builder <-
    return expr

  return
    ( LazyText.unpack ( Builder.toLazyText builder ) )



pair :: Dhall.Type a -> Dhall.Type b -> Dhall.Type ( a, b )
pair l r =
  makeRecord $ (,) <$> keyValue "_1" l <*> keyValue "_2" r



list :: Dhall.Type a -> Dhall.Type [a]
list t =
  toList <$> Dhall.vector t



newtype RecordBuilder a =
  RecordBuilder
    { unRecordBuilder ::
        Product
          ( Const
              ( Map.Map
                  LazyText.Text
                  ( Dhall.Expr Dhall.Parser.Src Dhall.TypeCheck.X )
              )
          )
          ( Compose
              ( Reader
                  ( Dhall.Expr Dhall.Parser.Src Dhall.TypeCheck.X )
              )
              Maybe
          )
          a
    }
  deriving (Functor, Applicative)



makeRecord :: RecordBuilder a -> Dhall.Type a
makeRecord ( RecordBuilder ( Pair ( Const fields ) ( Compose extractF ) ) ) =
  let
    extract =
      runReader extractF

    expected =
      Expr.Record fields

  in Dhall.Type { .. }



keyValue :: LazyText.Text -> Dhall.Type a -> RecordBuilder a
keyValue key valueType =
  let
    extract expr = do
      Expr.RecordLit fields <-
        return expr

      Map.lookup key fields >>= Dhall.extract valueType

  in
    RecordBuilder
      ( Pair
          ( Const ( Map.singleton key ( Dhall.expected valueType ) ) )
          ( Compose ( reader extract ) )
      )



emptyRecord :: Dhall.Type ()
emptyRecord =
  let
    extract expr = do
      Expr.RecordLit fields <-
        return expr

      guard ( Map.null fields )

    expected =
      Expr.Record Map.empty
      
  in Dhall.Type { .. }



makeUnion :: Map.Map LazyText.Text ( Dhall.Type a ) -> Dhall.Type a
makeUnion alts =
  let
    extract expr = do
      Expr.UnionLit ctor v _ <-
        return expr

      t <-
        Map.lookup ctor alts

      Dhall.extract t v

    expected =
      Expr.Union ( Dhall.expected <$> alts )

  in Dhall.Type { .. }



validateType :: Dhall.Type ( Maybe a ) -> Dhall.Type a
validateType a =
  a { Dhall.extract = join . Dhall.extract a }
