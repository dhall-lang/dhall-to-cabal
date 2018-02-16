{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Version where

import qualified Dhall.Core as Dhall ( Expr )
import qualified Dhall.Core as Expr ( Expr(..), Var(..) )


pattern Type :: Dhall.Expr s a
pattern Type =
  Expr.Var ( Expr.V "Version" 0 )



pattern V :: Dhall.Expr s a
pattern V =
  Expr.Var ( Expr.V "v" 0 )
