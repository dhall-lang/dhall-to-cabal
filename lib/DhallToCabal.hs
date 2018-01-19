{-# language DeriveFunctor #-}
{-# language OverloadedStrings #-}

module DhallToCabal where

import Data.Vector ( Vector )
import Unsafe.Coerce ( unsafeCoerce )

import qualified Data.Map as Map
import qualified Data.Vector
import qualified Dhall ( Type(..), lazyText, natural )
import qualified Dhall.Core as Dhall ( Expr )
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.TypeCheck as Dhall
import qualified Distribution.Version as Cabal
import qualified Dhall.Parser



-- Like Dhall.Type, but allows Embed Cabal.VersionRange

data Type a
  = Type
      { extract :: Dhall.Expr Dhall.Parser.Src Cabal.VersionRange -> Maybe a
      , expected :: Dhall.Expr Dhall.Parser.Src Dhall.X
      }
  deriving ( Functor )



adapt :: Dhall.Type a -> Type a
adapt t =
  Type
    { extract = \expr ->
        case expr of
          Expr.Embed _ ->
            Nothing

          _ ->
            Dhall.extract t ( unsafeCoerce expr )

    , expected =
      fmap Dhall.absurd ( Dhall.expected t)
    }


vector :: Type a -> Type (Vector a)
vector (Type extractIn expectedIn) = Type extractOut expectedOut
  where
    extractOut (Expr.ListLit _ es) = traverse extractIn es
    extractOut  _                  = Nothing

    expectedOut = Expr.App Expr.List expectedIn



maybe :: Type a -> Type (Maybe a)
maybe (Type extractIn expectedIn) = Type extractOut expectedOut
  where
    extractOut (Expr.OptionalLit _ es) = traverse extractIn es'
      where
        es' = if Data.Vector.null es then Nothing else Just (Data.Vector.head es)
    extractOut _ = Nothing

    expectedOut = Expr.App Expr.Optional expectedIn


pair :: Type a -> Type b -> Type ( a, b ) 
pair l r = Type extractOut expectedOut
  where
    extractOut (Expr.RecordLit elems) = do
      (,) <$> ( Map.lookup "_1" elems >>= extract l )
          <*> ( Map.lookup "_2" elems >>= extract r )

    expectedOut =
      Expr.Record
        ( Map.fromList
            [ ( "_1", expected l )
            , ( "_2", expected r )
            ]
        )

lazyText =
  adapt Dhall.lazyText

natural =
  adapt Dhall.natural
