{-# language ApplicativeDo #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Dhall.Extra
  ( validateType
  , sortExpr
  )
  where

import qualified Dhall
import qualified Dhall.Core as Dhall ( Expr )
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Map as Map


validateType :: Dhall.Type ( Maybe a ) -> Dhall.Type a
validateType a =
  a { Dhall.extract =
        \expr ->
          case Dhall.toMonadic (Dhall.extract a expr) of
            Left extractErrors -> Dhall.fromMonadic (Left extractErrors)
            Right Nothing -> Dhall.extractError "Validation failed"
            Right (Just ok) -> pure ok
          }


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
