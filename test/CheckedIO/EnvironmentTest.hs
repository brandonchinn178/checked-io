{- AUTOCOLLECT.TEST -}
{-# LANGUAGE LambdaCase #-}

module CheckedIO.EnvironmentTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import CheckedIO.Environment
import Test.Tasty.HUnit

{----- lookupEnv -----}

test =
  testCase "lookupEnv returns Just for existing env var" $
    uncheckIOE (lookupEnv "USER") >>= \case
      Just _ -> return ()
      Nothing -> assertFailure "$USER did not exist"

test =
  testCase "lookupEnv returns Nothing for non-existing env var" $ do
    res <- uncheckIOE (lookupEnv "does-not-exist")
    res @?= Nothing

{----- getEnv ----}

test =
  testCase "getEnv returns existing env var" $ do
    _ <- uncheckIOE (getEnv "USER")
    pure ()

test =
  testCase "getEnv throws for non-existing env var" $ do
    res <- uncheckUIO (try $ getEnv "does-not-exist")
    res @?= Left (GetEnvVarMissing "does-not-exist")
