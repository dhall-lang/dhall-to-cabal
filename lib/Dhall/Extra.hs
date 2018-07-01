{-# language ApplicativeDo #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}

module Dhall.Extra
  ( RecordBuilder
  , keyValue
  , makeRecord
  , makeUnion
  , validateType
  , sortExpr
  )
  where

import Control.Applicative ( Const(..) )
import Control.Monad ( join )
import Control.Monad.Trans.Reader ( Reader, reader, runReader )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Product ( Product(..) )
import Data.Hashable ( Hashable )
import Data.List ( sortBy )
import Data.Ord ( comparing )

import qualified Data.HashMap.Strict.InsOrd as Map
import qualified Data.Text as StrictText
import qualified Dhall
import qualified Dhall.Core as Dhall ( Expr )
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Parser
import qualified Dhall.TypeCheck 



newtype RecordBuilder a =
  RecordBuilder
    ( Product
        ( Const
            ( Map.InsOrdHashMap
                StrictText.Text
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
    )
  deriving (Functor, Applicative)



makeRecord :: RecordBuilder a -> Dhall.Type a
makeRecord ( RecordBuilder ( Pair ( Const fields ) ( Compose extractF ) ) ) =
  let
    extract =
      runReader extractF

    expected =
      sortExpr ( Expr.Record fields )

  in Dhall.Type { .. }



keyValue :: StrictText.Text -> Dhall.Type a -> RecordBuilder a
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



makeUnion :: Map.InsOrdHashMap StrictText.Text ( Dhall.Type a ) -> Dhall.Type a
makeUnion alts =
  let
    extract expr = do
      Expr.UnionLit ctor v _ <-
        return expr

      t <-
        Map.lookup ctor alts

      Dhall.extract t v

    expected =
      sortExpr ( Expr.Union ( Dhall.expected <$> alts ) )

  in Dhall.Type { .. }



validateType :: Dhall.Type ( Maybe a ) -> Dhall.Type a
validateType a =
  a { Dhall.extract = join . Dhall.extract a }


sortInsOrdHashMap :: ( Hashable k, Ord k ) => Map.InsOrdHashMap k v -> Map.InsOrdHashMap k v
sortInsOrdHashMap =
  Map.fromList . sortBy ( comparing fst ) . Map.toList


sortExpr :: Dhall.Expr s a -> Dhall.Expr s a
sortExpr = \case
  Expr.RecordLit r ->
    Expr.RecordLit ( sortInsOrdHashMap r )

  Expr.Record r ->
    Expr.Record ( sortInsOrdHashMap r )

  Expr.Union r ->
    Expr.Union ( sortInsOrdHashMap r )

  e ->
    e
