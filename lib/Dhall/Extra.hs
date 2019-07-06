{-# language ApplicativeDo #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}

module Dhall.Extra
  ( validateType
  , sortExpr
  )
  where

import Control.Monad ( join )
import Data.List ( sortBy )
import Data.Ord ( comparing )

import qualified Dhall
import qualified Dhall.Core as Dhall ( Expr )
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Map as Map


validateType :: Dhall.Type ( Maybe a ) -> Dhall.Type a
validateType a =
  a { Dhall.extract = join . Dhall.extract a }


sortExpr :: Dhall.Expr s a -> Dhall.Expr s a
sortExpr = \case
  Expr.RecordLit r ->
    Expr.RecordLit ( Map.sort r )

  Expr.Record r ->
    Expr.Record ( Map.sort r )

  Expr.Union r ->
    Expr.Union ( Map.sort r )

  e ->
    e
