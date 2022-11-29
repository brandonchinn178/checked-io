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
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import CheckedIO.IORef

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
    \(AnException e) -> ioProperty . uncheckUIO $ do
      expected <- (pure . Left @_ @()) e
      actual <- (try . throw) e
      pure (actual === expected)

{----- Bracket tests -----}

test =
  testCase "bracket runs clean up after successful action" $ do
    refs <- uncheckUIO $ do
      ref <- newIORef []
      let store x = modifyIORef' ref (x :)
      bracket (store "start") (\_ -> store "end") (\_ -> store "action")
      reverse <$> readIORef ref

    refs @?= ["start", "action", "end"]

test =
  testCase "bracket runs clean up after exception" $ do
    refs <- uncheckUIO $ do
      ref <- newIORef []
      let store x = modifyIORef' ref (x :)
      _ <-
        try . bracket (store "start") (\_ -> store "end") $ \_ -> do
          _ <- throw (GHC.userError "bad")
          store "action"
      reverse <$> readIORef ref

    refs @?= ["start", "end"]
