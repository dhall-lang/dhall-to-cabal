{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language TypeOperators #-}

module DhallToCabal.Diff ( Diffable(..) ) where

import Data.List ( (\\), intersect )

import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Types.ExecutableScope as Cabal
import qualified Distribution.Types.ForeignLib as Cabal
import qualified Distribution.Types.ForeignLibType as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified GHC.Generics as Generic



diffEqVia :: ( Monoid a, Eq b ) => ( a -> b ) -> a -> a -> ( b, b, b )
diffEqVia f left right =
  if f left == f right then
    ( f left, f mempty, f mempty )
  else
    ( f mempty, f left, f right )



class Diffable a where
  diff :: a -> a -> ( a, a, a )
  default diff :: ( Generic.Generic a, GDiffable ( Generic.Rep a ) ) => a -> a -> ( a, a, a )
  diff a b =
    let
      ( common, left, right ) =
        gdiff ( Generic.from a ) ( Generic.from b )

    in
      ( Generic.to common, Generic.to left, Generic.to right )



instance Diffable Cabal.BuildInfo where
  diff a b =
    let
      ( commonBuildable, leftBuildable, rightBuildable ) =
        diffEqVia Cabal.buildable a b

      ( common, left, right ) =
        case gdiff ( Generic.from a ) ( Generic.from b ) of
          ( common, left, right ) ->
            ( Generic.to common, Generic.to left, Generic.to right )

    in
      ( common { Cabal.buildable = commonBuildable }
      , left { Cabal.buildable = leftBuildable }
      , right { Cabal.buildable = rightBuildable }
      )



instance Diffable Cabal.Library where
  diff a b =
    let
      ( commonLibExposed, leftLibExposed, rightLibExposed ) =
        diffEqVia Cabal.libExposed a b

      ( common, left, right ) =
        case gdiff ( Generic.from a ) ( Generic.from b ) of
          ( common, left, right ) ->
            ( Generic.to common, Generic.to left, Generic.to right )

    in
      ( common { Cabal.libExposed = commonLibExposed }
      , left { Cabal.libExposed = leftLibExposed }
      , right { Cabal.libExposed = rightLibExposed }
      )


instance Diffable Cabal.Benchmark



instance Diffable Cabal.TestSuite



instance Diffable Cabal.Executable where
  diff a b =
    let
      ( commonModulePath, leftModulePath, rightModulePath ) =
        diffEqVia Cabal.modulePath a b

      ( common, left, right ) =
        case gdiff ( Generic.from a ) ( Generic.from b ) of
          ( common, left, right ) ->
            ( Generic.to common, Generic.to left, Generic.to right )

    in
      ( common { Cabal.modulePath = commonModulePath }
      , left { Cabal.modulePath = leftModulePath }
      , right { Cabal.modulePath = rightModulePath }
      )



instance Diffable Cabal.ForeignLib



instance Eq a => Diffable ( Maybe a ) where
  diff left right =
    if left == right then
      ( left, Nothing, Nothing )
    else
      ( Nothing, left, right )



instance Diffable Cabal.UnqualComponentName where
  diff left right =
    if left == right then
      ( left, mempty, mempty )
    else
      ( mempty, left, right )



instance Diffable Cabal.BenchmarkInterface where
  diff left right =
    if left == right then
      ( left, mempty, mempty )
    else
      ( mempty, left, right )



instance Diffable Cabal.ForeignLibType where
  diff left right =
    if left == right then
      ( left, mempty, mempty )
    else
      ( mempty, left, right )



instance Diffable Cabal.TestSuiteInterface where
  diff left right =
    if left == right then
      ( left, mempty, mempty )
    else
      ( mempty, left, right )



instance Diffable Cabal.ExecutableScope where
  diff left right =
    if left == right then
      ( left, mempty, mempty )
    else
      ( mempty, left, right )



instance Eq a => Diffable [a] where
  diff a b =
    ( intersect a b
    , a \\ b
    , b \\ a
    )



instance Diffable Bool where
  diff left right =
    if left == right then
      ( left, True, True )
    else
      ( True, left, right )



class GDiffable f where
  gdiff :: f a -> f a -> ( f a, f a, f a )



instance GDiffable f => GDiffable ( Generic.M1 i c f ) where
  gdiff ( Generic.M1 a ) ( Generic.M1 b ) =
    let
      ( common, left, right ) =
        gdiff a b

    in
      ( Generic.M1 common, Generic.M1 left, Generic.M1 right )



instance ( GDiffable f, GDiffable g ) => GDiffable ( f Generic.:*: g ) where
  gdiff ( a Generic.:*: x ) ( b Generic.:*: y ) =
    let
      ( common0, left0, right0 ) =
        gdiff a b

      ( common1, left1, right1 ) =
        gdiff x y

    in
      ( common0 Generic.:*: common1, left0 Generic.:*: left1, right0 Generic.:*: right1 )



instance Diffable a => GDiffable ( Generic.K1 i a ) where
  gdiff ( Generic.K1 a ) ( Generic.K1 b ) =
    let
      ( common, left, right ) =
        diff a b

    in
      ( Generic.K1 common, Generic.K1 left, Generic.K1 right )
