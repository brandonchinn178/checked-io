{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CheckedIO.Exception (
  Exception (..),
  mapError,
  liftError,

  -- * Synchronous exceptions
  throw,
  catch,
  try,

  -- * Asynchronous exceptions
  throwTo,

  -- * Imprecise exceptions
  throwImprecise,

  -- * Converting exceptions
  ConvertException (..),
  convertE,

  -- * Exception supertypes
  SomeException (..),
  SomeSyncException (..),
  SomeAsyncException (..),
) where

import Control.Exception (
  Exception (..),
  SomeAsyncException (..),
 )

import CheckedIO.Core
import CheckedIO.Prelude.NoIO

-- | A type class for converting one exception type to another.
class (Exception e1, Exception e2) => ConvertException e1 e2 where
  convertException :: e1 -> e2
instance {-# OVERLAPPABLE #-} Exception e => ConvertException e e where
  convertException = id

convertE :: ConvertException e1 e2 => IOE e1 a -> IOE e2 a
convertE = mapError convertException
