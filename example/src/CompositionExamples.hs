{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module CompositionExamples where

import CheckedIO.Environment (GetEnvVarError, getEnv, lookupEnvIO)
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)

data MyException = MyException
  deriving stock (Show)
  deriving anyclass (Exception)

data MyException2 = MyException2
  deriving stock (Show)
  deriving anyclass (Exception)

{-- Lightweight Exception Handling --}

newtype CheckedIO a = CheckedIO (IO a)
  deriving newtype (Functor, Applicative, Monad)

class Throws e

unCheck :: CheckedIO a -> IO a
unCheck (CheckedIO m) = m

toChecked :: (Exception e, Throws e) => IOE e a -> CheckedIO a
toChecked = CheckedIO . liftE

throwChecked1 :: (Exception e, Throws e) => e -> CheckedIO a
throwChecked1 = CheckedIO . throw . SomeSyncException

f1 :: Throws GetEnvVarError => CheckedIO String
f1 = toChecked $ getEnv "USER"

g1 :: Throws MyException => String -> CheckedIO ()
g1 = undefined

-- type checks
h1 :: (Throws GetEnvVarError, Throws MyException) => CheckedIO ()
h1 = f1 >>= g1

-- type checks, can't infer
foo1 :: (Throws MyException, Throws MyException2) => String -> CheckedIO ()
foo1 "bad" = throwChecked1 MyException
foo1 "bad2" = throwChecked1 MyException2
foo1 _ = pure ()

{-- plucky --}

data EitherE e1 e2 = LeftE e1 | RightE e2
  deriving stock (Show)
  deriving anyclass (Exception)

class (Exception large, Exception single) => ProjectError large single where
  putError :: single -> large
instance {-# OVERLAPPABLE #-} Exception a => ProjectError a a where
  putError = id
instance {-# OVERLAPPING #-} (Exception a, Exception b) => ProjectError (EitherE a b) a where
  putError = LeftE
instance {-# OVERLAPPABLE #-} (Exception a, ProjectError b c) => ProjectError (EitherE a b) c where
  putError = RightE . putError

toPlucked :: ProjectError e' e => IOE e a -> IOE e' a
toPlucked = mapExceptionM putError

throwPlucked :: ProjectError e' e => e -> IOE e' a
throwPlucked = throw . putError

catchOne ::
  (Exception e, Exception e') =>
  IOE (EitherE e' e) a ->
  (e -> IOE e' a) ->
  IOE e' a
catchOne m f = m `catch` go
  where
    go = \case
      LeftE e -> throw e
      RightE e -> f e

f2 :: ProjectError e GetEnvVarError => IOE e String
f2 = toPlucked $ getEnv "USER"

g2 :: ProjectError e MyException => String -> IOE e ()
g2 = undefined

-- type checks
h2 :: (ProjectError e GetEnvVarError, ProjectError e MyException) => IOE e ()
h2 = f2 >>= g2

-- ghci infers:
--   (ProjectError e' MyException, ProjectError e' MyException2) =>
--   String -> IOE e' ()
foo2 "bad" = throwPlucked MyException
foo2 "bad2" = throwPlucked MyException2
foo2 _ = pure ()

{-- control-monad-exception + exceptions-checked --}

data Caught e l

class Exception e => Throws3 e l
instance (Throws3 e l, Exception e') => Throws3 e (Caught e' l)
instance Exception e => Throws3 e (Caught e l)

newtype CheckedIO3 l a = CheckedIO3 {unCheckedIO3 :: IO a}
  deriving newtype (Functor, Applicative, Monad)

toChecked3 :: Throws3 e l => IOE e a -> CheckedIO3 l a
toChecked3 = CheckedIO3 . mapExceptionM SomeSyncException

throwChecked3 :: Throws3 e l => e -> CheckedIO3 l a
throwChecked3 = CheckedIO3 . throw . SomeSyncException

catchOne3 ::
  Exception e =>
  CheckedIO3 (Caught e l) a ->
  (e -> CheckedIO3 l a) ->
  CheckedIO3 l a
catchOne3 (CheckedIO3 m) f = CheckedIO3 $ m `catch` (unCheckedIO3 . f . fromJust . fromSyncException)

f3 :: Throws3 GetEnvVarError e => CheckedIO3 e String
f3 = toChecked3 $ getEnv "USER"

g3 :: Throws3 MyException e => String -> CheckedIO3 e ()
g3 = undefined

-- type checks
h3 :: (Throws3 GetEnvVarError e, Throws3 MyException e) => CheckedIO3 e ()
h3 = f3 >>= g3

-- ghci infers:
--   (Throws3 MyException l, Throws3 MyException2 l) =>
--   String -> CheckedIO3 l ()
foo3 "bad" = throwChecked3 MyException
foo3 "bad2" = throwChecked3 MyException2
foo3 _ = pure ()
