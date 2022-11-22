{-|
Same as @CheckedIO.Prelude@, plus some more re-exported functions
for convenience. This module might be useful to use as a new
@Prelude@.

@
\{\-# LANGUAGE NoImplicitPrelude #\-\}
import CheckedIO
@

Or with Cabal mixins:

@
mixins:
    base hiding (Prelude)
  , checked-io (CheckedIO as Prelude)
@
-}
module CheckedIO (
  -- * CheckedIO.Prelude
  module CheckedIO.Prelude,

  -- * CheckedIO.Core
  module CheckedIO.Core,

  -- * Other re-exports
  module X,
) where

import CheckedIO.Core
import CheckedIO.Exception as X (
  Exception (..),
  convertE,
 )
import CheckedIO.Prelude
