{-# language ViewPatterns #-}

module DhallToCabal.Util
  ( relativeTo
  )
  where

import System.FilePath
  ( dropTrailingPathSeparator, joinPath, normalise, splitDirectories, takeDirectory )

-- | Like 'System.FilePath.makeRelative', but will introduce @..@
-- segments (and hence will misbehave in the presence of symlinks).
--
-- If the path being relativised is identical to the root path, then
-- this will return the empty string.
relativeTo
  :: FilePath
     -- ^ The path to be relative to. Note that the final file-name is
     -- ignored: @foo/bar@ is relative to @foo/@, even if @foo/bar@ is
     -- a directory.
  -> FilePath
     -- ^ The path to relativise.
  -> FilePath
relativeTo =
  \ ( splitDirectories . dropTrailingPathSeparator . takeDirectory . normalise -> base ) ->
  \ ( splitDirectories . normalise -> path ) ->
      joinPath ( go base path )
  where
  -- @normalise "."@ is @"."@, so we have to take care here with dots.
  go ( a : as ) ( b : bs )
    | a == b = go as bs
    | a == "." = go as ( b : bs )
    | b == "." = go (a : as) bs
    | otherwise = ( ".." <$ ( a : as ) ) ++ ( b : bs )
  go [] bs = bs
  go as [] = ".." <$ as
