{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module VersionRange where

import qualified Dhall.Core as Dhall ( Expr )
import qualified Dhall.Core as Expr ( Expr(..), Var(..) )


pattern Type :: Dhall.Expr s a
pattern Type =
  Expr.Var ( Expr.V "VersionRange" 0 )



pattern AnyVersion :: Dhall.Expr s a
pattern AnyVersion =
  Expr.Var ( Expr.V "anyVersion" 0 )



pattern NoVersion :: Dhall.Expr s a
pattern NoVersion =
  Expr.Var ( Expr.V "noVersion" 0 )



pattern ThisVersion :: Dhall.Expr s a
pattern ThisVersion =
  Expr.Var ( Expr.V "thisVersion" 0 )



pattern NotThisVersion :: Dhall.Expr s a
pattern NotThisVersion =
  Expr.Var ( Expr.V "notThisVersion" 0 )



pattern LaterVersion :: Dhall.Expr s a
pattern LaterVersion =
  Expr.Var ( Expr.V "laterVersion" 0 )



pattern EarlierVersion :: Dhall.Expr s a
pattern EarlierVersion =
  Expr.Var ( Expr.V "earlierVersion" 0 )



pattern OrLaterVersion :: Dhall.Expr s a
pattern OrLaterVersion =
  Expr.Var ( Expr.V "orLaterVersion" 0 )



pattern OrEarlierVersion :: Dhall.Expr s a
pattern OrEarlierVersion =
  Expr.Var ( Expr.V "orEarlierVersion" 0 )



pattern WithinVersion :: Dhall.Expr s a
pattern WithinVersion =
  Expr.Var ( Expr.V "withinVersion" 0 )



pattern MajorBoundVersion :: Dhall.Expr s a
pattern MajorBoundVersion =
  Expr.Var ( Expr.V "majorBoundVersion" 0 )



pattern UnionVersionRanges :: Dhall.Expr s a
pattern UnionVersionRanges =
  Expr.Var ( Expr.V "unionVersionRanges" 0 )



pattern IntersectVersionRanges :: Dhall.Expr s a
pattern IntersectVersionRanges =
  Expr.Var ( Expr.V "intersectVersionRanges" 0 )



pattern DifferenceVersionRanges :: Dhall.Expr s a
pattern DifferenceVersionRanges =
  Expr.Var ( Expr.V "differenceVersionRanges" 0 )



pattern InvertVersionRange :: Dhall.Expr s a
pattern InvertVersionRange =
  Expr.Var ( Expr.V "invertVersionRange" 0 )
