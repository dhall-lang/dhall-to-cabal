module Dhall.RemoveDefault ( subtractRecordFields ) where

import Control.Monad ( guard )

import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap

-- | Given a record 'Expr' that the user has written, try and remove any
-- settings that are equal to those in a record of defaults.
--
-- If the right record contains fields that aren't in the left, 'Nothing' is
-- returned.
--
-- If there are no fields in common, 'Nothing' is returned.
--
-- If all fields are equal, the empty map is returned.
subtractRecordFields left right = do
  let
    intersection =
      InsOrdHashMap.intersectionWith (==) left right

  -- The right record cannot have any fields not in left.
  guard ( InsOrdHashMap.null ( InsOrdHashMap.difference right left ) )

  -- We must have at least one field with a common name
  guard ( not ( InsOrdHashMap.null intersection ) )

  let
    extra =
      InsOrdHashMap.mapMaybe
        id
        ( InsOrdHashMap.intersectionWith
            ( \a b ->
                if a /= b then
                  Just a
                else
                  Nothing
            )
            left
            right
        )

  return extra
