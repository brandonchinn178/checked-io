{-|
Same as @Prelude@, except using the new 'IO' type.
-}
module CheckedIO.Prelude (
  -- * CheckedIO.Prelude.NoIO
  module CheckedIO.Prelude.NoIO,

  -- * CheckedIO.Core
  module CheckedIO.Core,

  -- * Reimplemented prelude functions
  putStrLn,
  getLine,
) where

import qualified Prelude

import CheckedIO.Core (IO)
import qualified CheckedIO.Core as Core
import CheckedIO.Prelude.NoIO

putStrLn :: String -> IO ()
putStrLn = Core.checkIO . Prelude.putStrLn

getLine :: IO String
getLine = Core.checkIO Prelude.getLine
