{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module CheckedIO.Core (
  -- * UIO: Checked IO without exceptions
  UIO (UnsafeUIO),
  MonadUIO (..),
  MonadUnliftUIO (..),

  -- * IOE: Checked IO with exceptions
  IOE (UnsafeIOE),
  MonadIOE (..),
  MonadUnliftIOE (..),

  -- * IO convenience type
  IO,
  liftIO,
  withRunInIO,

  -- * IOE operations
  mapError,
  liftError,
  throw,
  catch,
  try,

  -- * Interop with unchecked IO
  Main,
  UnsafeIO,
  checkIO,
  unsafeCheckIO,
  unsafeCheckUIO,
  uncheckIOE,
  uncheckUIO,

  -- * Exceptions in checked IO actions
  ExceptionType (..),
  SomeException (..),
  SomeSyncException (..),
) where

import Control.Exception (Exception (..))
import qualified Control.Exception as GHC
import qualified Control.Exception.Base as GHC
import Data.Bifunctor (first)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Typeable (cast)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified System.IO as GHC

import CheckedIO.Prelude.NoIO

{----- UIO -----}

-- | A checked IO action that can not throw any (synchronous) exceptions.
newtype UIO a = UnsafeUIO {unUIO :: GHC.IO a}
  deriving (Functor, Applicative, Monad)

-- | Lift a 'UIO' action to the given monad.
class Monad m => MonadUIO m where
  liftUIO :: UIO a -> m a
instance MonadUIO UIO where
  liftUIO = id

-- | Provide a function for running an action in the given monad in 'UIO'.
--
-- Must satisfy the idempotency law:
--
-- @
-- withRunInUIO (\run -> run m) === m
-- @
class MonadUIO m => MonadUnliftUIO m where
  withRunInUIO :: ((forall a. m a -> UIO a) -> UIO b) -> m b
instance MonadUnliftUIO UIO where
  withRunInUIO f = f id

{----- IOE -----}

-- | A checked IO action that can only throw synchronous exceptions
-- of the given type.
--
-- Morally equivalent to `UIO (Either e a)`, but implemented without
-- the `Either` for performance.
newtype IOE e a = UnsafeIOE {unIOE :: GHC.IO a}
  deriving (Functor, Applicative, Monad)

instance MonadUIO (IOE e) where
  liftUIO = UnsafeIOE . unUIO

toUIO :: (HasCallStack, Exception e) => IOE e a -> UIO (Either e a)
toUIO m = (Right <$> m) `catchUIO` (pure . Left)

fromUIO :: Exception e => UIO (Either e a) -> IOE e a
fromUIO m = liftUIO m >>= either throw pure

-- | Lift an 'IOE' action to the given monad.
--
-- e.g. to convert an 'IOE' action to an 'ExceptT' stack on top of 'UIO':
--
-- @
-- newtype ExceptT e m a = ExceptT { unExceptT :: m (Either e a) }
--   deriving (Functor, Applicative, Monad)
--
-- instance MonadUIO m => MonadIOE e (ExceptT e m) where
--   liftIOE = ExceptT . liftUIO . toUIO
-- @
class Monad m => MonadIOE e m where
  liftIOE :: IOE e a -> m a
instance MonadIOE e (IOE e) where
  liftIOE = id

-- | Provide a function for running an action in the given monad in 'IOE'.
--
-- Must satisfy the idempotency law:
--
-- @
-- withRunInIOE (\run -> (liftIOE . run) m) === m
-- @
class MonadIOE e m => MonadUnliftIOE e m where
  withRunInIOE :: ((forall a. m a -> IOE e a) -> IOE e b) -> m b
instance MonadUnliftIOE e (IOE e) where
  withRunInIOE f = f id

{----- IO convenience type -----}

-- | A helper for most IO actions in `base`.
type IO = IOE GHC.IOException

-- | 'liftIOE' specialized to 'IO'
liftIO :: MonadIOE GHC.IOException m => IO a -> m a
liftIO = liftIOE

-- | 'withRunInIOE' specialized to 'IO'
withRunInIO ::
  MonadUnliftIOE GHC.IOException m =>
  ((forall a. m a -> IO a) -> IO b)
  -> m b
withRunInIO = withRunInIOE

{----- IOE operations -----}

mapError :: (Exception e1, Exception e2) => (e1 -> e2) -> IOE e1 a -> IOE e2 a
mapError f = fromUIO . fmap (first f) . toUIO

liftError :: Exception e => IOE e a -> IOE SomeSyncException a
liftError = mapError SomeSyncException

throw :: Exception e => e -> IOE e a
throw = UnsafeIOE . GHC.throwIO . SomeException SyncExceptionType

-- | If your handler does not throw an error, consider using 'catchUIO'.
catch :: (HasCallStack, Exception e1, Exception e2) => IOE e1 a -> (e1 -> IOE e2 a) -> IOE e2 a
catch m f = fromUIO $ (Right <$> m) `catchUIO` (toUIO . f)

-- | Same as 'toUIO', except returns as an 'IOE'.
try :: Exception e => IOE e a -> IOE e' (Either e a)
try = liftUIO . toUIO

catchUIO :: (HasCallStack, Exception e) => IOE e a -> (e -> UIO a) -> UIO a
catchUIO (UnsafeIOE m) f =
  UnsafeUIO $
    m `GHC.catch` \case
      SomeException SyncExceptionType e ->
        case cast e of
          Just e' -> unUIO (f e')
          Nothing ->
            error $
              "checked-io invariant violation: IOE contained an unexpected synchronous exception: "
                ++ show e
      e -> GHC.throwIO e

{----- Interop with unchecked IO -----}

-- | A convenience alias for 'main' functions
type Main = UnsafeIO ()

-- | An alias for the normal unsafe 'GHC.IO' type.
type UnsafeIO = GHC.IO

-- | Convert an unchecked IO action into a checked IO action.
checkIO :: UnsafeIO a -> IOE SomeSyncException a
checkIO = UnsafeIOE . GHC.handle (GHC.throwIO . convert)
  where
    convert = \case
      SomeException SyncExceptionType e ->
        SomeException SyncExceptionType (SomeSyncException e)
      e -> e

-- | Convert an unchecked IO action into a checked IO action that can throw
-- the given exception type.
--
-- If the IO action threw a different synchronous exception, this function
-- will error.
unsafeCheckIO :: (HasCallStack, Exception e) => UnsafeIO a -> IOE e a
unsafeCheckIO = mapError convert . checkIO
  where
    convert (SomeSyncException e) =
      case cast e of
        Just e' -> e'
        Nothing ->
          withFrozenCallStack . error $
            "unsafeCheckIO was called on an action that threw an unexpected error: "
              ++ show e

unsafeCheckUIO :: HasCallStack => UnsafeIO a -> UIO a
unsafeCheckUIO = fmap (either foundError id) . toUIO . checkIO
  where
    foundError (SomeSyncException e) =
      withFrozenCallStack . error $
        "unsafeCheckUIO was called on an action that unexpectedly threw an error: "
          ++ show e

uncheckIOE :: IOE e a -> UnsafeIO a
uncheckIOE = GHC.handle go . unIOE
  where
    go (SomeException _ e) = GHC.throwIO e

uncheckUIO :: UIO a -> UnsafeIO a
uncheckUIO = GHC.handle go . unUIO
  where
    go (SomeException _ e) = GHC.throwIO e

{----- Exceptions in IOE/UIO -----}

-- TODO: should ExitCode be handled specially?
data ExceptionType
  = SyncExceptionType
  | AsyncExceptionType
  | ImpreciseExceptionType
  deriving (Show, Eq)

-- | All exceptions floating around 'UIO' or 'IOE'
-- (synchronous or otherwise) will be a 'SomeException'.
data SomeException =
  forall e. Exception e =>
  SomeException ExceptionType e

deriving instance Show SomeException

instance Exception SomeException where
  fromException = Just . toSomeException

toSomeException :: GHC.SomeException -> SomeException
toSomeException someE@(GHC.SomeException e) =
  fromMaybe (asSync e) . asum $
    [ cast e
    , asImprecise <$> castE @GHC.ErrorCall
    , asImprecise <$> castE @GHC.TypeError
    , asImprecise <$> castE @GHC.ArithException
    , asImprecise <$> castE @GHC.ArrayException
    , asImprecise <$> castE @GHC.AssertionFailed
    , asImprecise <$> castE @GHC.NestedAtomically
    , asImprecise <$> castE @GHC.NoMethodError
    , asImprecise <$> castE @GHC.PatternMatchFail
    , asImprecise <$> castE @GHC.RecConError
    , asImprecise <$> castE @GHC.RecSelError
    , asImprecise <$> castE @GHC.RecUpdError
    , asAsync <$> castE @GHC.AsyncException
    , asAsync <$> castE @GHC.CompactionFailed
    , asAsync <$> castE @GHC.FixIOException
    , asAsync <$> castE @GHC.AsyncException
    , asAsync <$> castE @GHC.BlockedIndefinitelyOnSTM
    , asAsync <$> castE @GHC.BlockedIndefinitelyOnMVar
    , asAsync <$> castE @GHC.Deadlock
    , asAsync <$> castE @GHC.NonTermination
    , (\(GHC.SomeAsyncException e') -> asAsync e') <$> castE
    ]
  where
    asSync :: Exception e => e -> SomeException
    asSync = SomeException SyncExceptionType

    asAsync :: Exception e => e -> SomeException
    asAsync = SomeException AsyncExceptionType

    asImprecise :: Exception e => e -> SomeException
    asImprecise = SomeException ImpreciseExceptionType

    castE :: forall e. Exception e => Maybe e
    castE = fromException someE

data SomeSyncException = forall e. Exception e => SomeSyncException e

instance Show SomeSyncException where
  showsPrec p (SomeSyncException e) = showsPrec p e

instance Exception SomeSyncException
