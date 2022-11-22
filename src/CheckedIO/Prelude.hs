{-|
Same as @Prelude@, except using the new 'IO' type.
-}
module CheckedIO.Prelude (
  -- * CheckedIO.Prelude.NoIO
  module CheckedIO.Prelude.NoIO,

  -- * CheckedIO.Core
  module CheckedIO.Core,
) where

import CheckedIO.Core (IO)
import CheckedIO.Prelude.NoIO
