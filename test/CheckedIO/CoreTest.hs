{- AUTOCOLLECT.TEST -}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CheckedIO.CoreTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import qualified Control.Exception as GHC
import qualified GHC.IO.Exception as GHC
import Test.Tasty.QuickCheck

data AnException = forall e. (Exception e, Eq e) => AnException e

instance Show AnException where
  show (AnException e) = show e

instance Arbitrary AnException where
  arbitrary = AnException . GHC.userError . getPrintableString <$> arbitrary

{----- Interop tests ----}

test =
  testProperty "(uncheckIOE . checkIO) returns a result" $
    \(x :: Int) -> ioProperty $ do
      res <- (uncheckIOE . checkIO) (pure x)
      pure (res === x)

test =
  testProperty "(uncheckIOE . checkIO) throws the same exception" $
    \(AnException e) -> ioProperty $ do
      res <- GHC.try $ (uncheckIOE . checkIO) (GHC.throwIO e :: UnsafeIO ())
      pure (res === Left e)

{----- IOE tests -----}

test =
  testProperty "try . throw === pure . Left" $
    \(AnException e) -> ioProperty . uncheckIOE $ do
      expected <- (pure . Left @_ @()) e
      actual <- (try . throw) e
      pure (actual === expected)
