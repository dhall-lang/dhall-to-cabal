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
import Control.Monad ( guard, join )
import Control.Monad.Trans.Reader ( Reader, reader, runReader )
import Data.Foldable ( toList )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Product ( Product(..) )

import qualified Data.Map as Map
import qualified Data.Text.Lazy as LazyText
import qualified Dhall
import qualified Dhall.Core as Dhall ( Expr )
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Parser
import qualified Dhall.TypeCheck 



string :: Dhall.Type String
string =
  LazyText.unpack <$> Dhall.lazyText



pair :: Dhall.Type a -> Dhall.Type b -> Dhall.Type ( a, b )
pair l r =
  makeRecord $ (,) <$> keyValue "_1" l <*> keyValue "_2" r



list :: Dhall.Type a -> Dhall.Type [a]
list t =
  toList <$> Dhall.vector t



newtype RecordBuilder a =
  RecordBuilder
    ( Product
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
    )
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
