{-# language DeriveFunctor #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module DhallToCabal.ConfigTree ( ConfigTree(..), toConfigTree ) where

import Control.Applicative
import Control.Lens (deepOf)
import Data.Functor.Identity
import Data.Monoid
import Dhall.Core hiding (Const)



data ConfigTree cond a
  = Leaf a
  | Branch cond ( ConfigTree cond a ) ( ConfigTree cond a )
  deriving (Functor, Show)



toConfigTree :: ( Eq a, Eq s ) => Expr s a -> ConfigTree ( Expr s a ) ( Expr s a )
toConfigTree e =
  let
    v =
      "config"

    saturated =
      normalize (App e (Var v))

    loop e =
      let
        Const condition =
          deepOf
            subExpr
            ( findConfigUse v )
            ( Const . First . Just ) e

      in
        case condition of
          First Nothing ->
            Leaf e

          First (Just cond) ->
            let
              replaceWith x =
                loop
                  . normalize
                  . runIdentity
                  . deepOf
                      subExpr
                      ( exactly cond )
                      ( const ( pure x ) )

            in
              Branch
                cond
                ( replaceWith ( BoolLit True ) e )
                ( replaceWith ( BoolLit False ) e )

  in loop saturated



findConfigUse
  :: Applicative f
  => Var -> ( Expr s a -> f ( Expr s a ) ) -> Expr s a -> f ( Expr s a )
findConfigUse x f e@(App (Field (Var x') "os") _) | x == x' = f e
findConfigUse x f e@(App (Field (Var x') "arch") _) | x == x' = f e
findConfigUse x f e@(App (App (Field (Var x') "impl") _) _) | x == x' = f e
findConfigUse x f e@(App (Field (Var x') "flag") _) | x == x' = f e
findConfigUse _ _ e = pure e



exactly :: ( Applicative f, Eq a ) => a -> ( a -> f a ) -> a -> f a
exactly x f e | x == e = f e
exactly _ _ e = pure e



subExpr
  :: Applicative f
  => ( Expr s a -> f ( Expr s a ) ) -> Expr s a -> f ( Expr s a )
subExpr f = \case
  Lam a b c ->
    Lam a <$> f b <*> f c

  Pi a b c ->
    Pi a <$> f b <*> f c

  App a b ->
    App <$> f a <*> f b

  Let a b c d ->
    Let a <$> traverse f b <*> f c <*> f d

  Annot a b ->
    Annot <$> f a <*> f b

  BoolAnd a b ->
    BoolAnd <$> f a <*> f b

  BoolOr a b ->
    BoolOr <$> f a <*> f b

  BoolEQ a b ->
    BoolEQ <$> f a <*> f b

  BoolNE a b ->
    BoolNE <$> f a <*> f b

  BoolIf a b c ->
    BoolIf <$> f a <*> f b <*> f c

  NaturalPlus a b ->
    NaturalPlus <$> f a <*> f b

  NaturalTimes a b ->
    NaturalTimes <$> f a <*> f b

  TextLit (Chunks a b) ->
    TextLit
      <$>
        ( Chunks
            <$> traverse ( \(a,b) -> (,) <$> pure a <*> f b ) a <*> pure b
        )

  TextAppend a b ->
    TextAppend <$> f a <*> f b

  ListLit a b ->
    ListLit <$> traverse f a <*> traverse f b

  ListAppend a b ->
    ListAppend <$> f a <*> f b

  OptionalLit a b ->
    OptionalLit <$> f a <*> traverse f b

  Record a ->
    Record <$> traverse f a

  RecordLit a ->
    RecordLit <$> traverse f a

  Union a ->
    Union <$> traverse f a

  UnionLit a b c ->
    UnionLit a <$> f b <*> traverse f c

  Combine a b ->
    Combine <$> f a <*> f b

  Prefer a b ->
    Prefer <$> f a <*> f b

  Merge a b t ->
    Merge <$> f a <*> f b <*> traverse f t

  Constructors a ->
    Constructors <$> f a

  Field a b ->
    Field <$> f a <*> pure b

  Note a b ->
    Note a <$> f b

  e ->
    pure e
