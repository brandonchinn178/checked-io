{- AUTOCOLLECT.TEST -}

{-# LANGUAGE ScopedTypeVariables #-}

module CheckedIO.CoreTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import Test.Tasty.QuickCheck

test =
  testProperty "(uncheckIOE . checkIO) returns a result" $
    \(x :: Int) -> ioProperty $ do
      res <- (uncheckIOE . checkIO) (pure x)
      pure (res === x)
