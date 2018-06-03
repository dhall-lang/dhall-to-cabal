{-# language DeriveFunctor #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module DhallToCabal.ConfigTree ( ConfigTree(..), toConfigTree ) where

import Control.Monad
import Data.Semigroup ( Semigroup ( (<>) ) )
import Dhall.Core hiding ( Const )


-- | 'ConfigTree' captures a logic-monad like expansion of the result of
-- Bool-valued expressions.

data ConfigTree cond a
  = Leaf a
  | Branch cond ( ConfigTree cond a ) ( ConfigTree cond a )
  deriving (Functor, Show)

instance Applicative ( ConfigTree cond ) where
  pure = Leaf
  (<*>) = ap

instance Monad ( ConfigTree cond ) where
  return = pure

  Leaf a >>= f = f a
  Branch cond l r >>= f = Branch cond ( l >>= f ) ( r >>= f )

instance ( Semigroup a ) => Semigroup ( ConfigTree cond a ) where
  (<>) = liftM2 (<>)

instance ( Monoid a ) => Monoid ( ConfigTree cond a ) where
  mempty = pure mempty
  mappend = liftM2 mappend


-- | Given a Dhall expression that is of the form @λ( config : Config ) -> a@,
-- find all saturated uses of @config@, and substitute in either @True@ or
-- @False@. The two substitutions are captured in a 'Branch'.

toConfigTree
  :: ( Eq a )
  => Expr s a
  -> ConfigTree ( Expr s a ) ( Expr s a )
toConfigTree e =
  let
    v =
      "config"

    saturated =
      normalize ( App e ( Var v ) )

    loop e =
      case normalize <$> rewriteConfigUse v e of
        Leaf a ->
          Leaf a

        Branch cond l r ->
          Branch cond ( loop =<< l ) ( loop =<< r )

  in loop saturated



-- | Find all config-like uses of a given variable, and expand them into all
-- possible results of evaluation.

rewriteConfigUse :: Var -> Expr s a -> ConfigTree (Expr s a) (Expr s a)
rewriteConfigUse v =
 transformMOf
   subExpr
   ( \expr ->
       if isConfigUse expr then
         Branch
           expr
             ( pure ( BoolLit True ) )
             ( pure ( BoolLit False ) )
       else
         pure expr
   )

  where
    
    isConfigUse (App (Field (Var x') "os") _)           | v == x' = True
    isConfigUse (App (Field (Var x') "arch") _)         | v == x' = True
    isConfigUse (App (App (Field (Var x') "impl") _) _) | v == x' = True
    isConfigUse (App (Field (Var x') "flag") _)         | v == x' = True
    isConfigUse _ = False


-- | Transform every element in a tree using a user supplied 'Traversal' in a
-- bottom-up manner with a monadic effect.
transformMOf
  :: Monad m =>
  ( ( t -> m b ) -> t -> m a ) -> ( a -> m b ) -> t -> m b
transformMOf l f = go where
  go t = l go t >>= f
{-# INLINE transformMOf #-}



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
