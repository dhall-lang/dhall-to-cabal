{-# language DeriveFunctor #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module DhallToCabal.ConfigTree ( ConfigTree(..), toConfigTree ) where

import Control.Applicative
import Control.Lens (deepOf)
import Data.Maybe ( isNothing )
import Control.Monad ( when )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State.Strict
import Data.Functor.Identity
import Data.Functor.Product ( Product(Pair) )
import Dhall.Core hiding (Const)



data ConfigTree cond a
  = Leaf a
  | Branch cond ( ConfigTree cond a ) ( ConfigTree cond a )
  deriving (Functor, Show)



-- | Given a Dhall expression that is of the form @λ( config : Config ) -> a@,
-- find all saturated uses of @config@, and substitute in either @True@ or
-- @False@.

toConfigTree
  :: ( Eq a, Eq s )
  => Expr s a
  -> ConfigTree ( Expr s a ) ( Expr s a )
toConfigTree e =
  let
    v =
      "config"

    saturated =
      normalize ( App e ( Var v ) )

    loop e =
      let
        Pair ( Identity ( a, useA ) ) ( Identity ( b, useB ) ) =
          runStateT ( rewriteConfigUse e v ) Nothing

      in
        case useA <|> useB of
          Nothing ->
            Leaf e

          Just cond ->
            Branch cond ( loop ( normalize a ) ) ( loop ( normalize b ) )

  in loop saturated


rewriteConfigUse e v =
  deepOf
   subExpr
   ( findConfigUse v )
   ( \configUse -> do
       lastSeen <-
         get

       when ( isNothing lastSeen ) ( put ( Just configUse ) )

       case lastSeen of
         Just last | configUse /= last ->
           pure configUse

         _ ->
           lift
             ( Pair
                 ( pure ( BoolLit True ) )
                 ( pure ( BoolLit False ) )
             )
   )
   e


findConfigUse
  :: Applicative f
  => Var -> ( Expr s a -> f ( Expr s a ) ) -> Expr s a -> f ( Expr s a )
findConfigUse x f e@(App (Field (Var x') "os") _) | x == x' = f e
findConfigUse x f e@(App (Field (Var x') "arch") _) | x == x' = f e
findConfigUse x f e@(App (App (Field (Var x') "impl") _) _) | x == x' = f e
findConfigUse x f e@(App (Field (Var x') "flag") _) | x == x' = f e
findConfigUse _ _ e = pure e



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
