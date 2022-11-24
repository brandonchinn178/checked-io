{-|
'Prelude' without 'IO'
-}
module CheckedIO.Prelude.NoIO (
  module Prelude,
) where

import Prelude hiding (
  IO,
  getLine,
  putStrLn,
 )
