{-# language ApplicativeDo #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}

module Dhall.Extra
  ( makeUnion
  , validateType
  , sortExpr
  )
  where

import Control.Monad ( join )
import Data.List ( sortBy )
import Data.Ord ( comparing )

import qualified Data.Text as StrictText
import qualified Dhall
import qualified Dhall.Core as Dhall ( Expr )
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Map as Map


makeUnion :: Map.Map StrictText.Text ( Dhall.Type a ) -> Dhall.Type a
makeUnion alts =
  let
    extract expr = do
      Expr.UnionLit ctor v _ <-
        return expr

      t <-
        Map.lookup ctor alts

      Dhall.extract t v

    expected =
      sortExpr ( Expr.Union ( Just . Dhall.expected <$> alts ) )

  in Dhall.Type { .. }



validateType :: Dhall.Type ( Maybe a ) -> Dhall.Type a
validateType a =
  a { Dhall.extract = join . Dhall.extract a }


sortInsOrdHashMap :: Ord k => Map.Map k v -> Map.Map k v
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
