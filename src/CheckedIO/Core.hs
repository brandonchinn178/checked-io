{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  MonadIO,
  MonadUnliftIO,
  liftIO,
  withRunInIO,

  -- * Conversions
  toUIO,
  fromUIO,
  fromUIOWith,
  uioToIO,

  -- * IOE operations
  mapExceptionM,
  liftE,
  throw,
  throwTo,
  throwImprecise,
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

import Control.Concurrent (ThreadId)
import Control.Exception (Exception (..))
import qualified Control.Exception as GHC
import qualified Control.Exception.Base as GHC
import Data.Bifunctor (first)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Typeable (cast)
import Data.Void (Void, absurd)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified System.IO as GHC

import CheckedIO.Prelude.NoIO

-- | An alias for the normal unsafe 'GHC.IO' type.
type UnsafeIO = GHC.IO

-- | A checked IO action that can not throw any (synchronous) exceptions.
newtype UIO a = UnsafeUIO {unUIO :: GHC.IO a}
  deriving (Functor, Applicative, Monad)

-- | A checked IO action that can only throw synchronous exceptions
-- of the given type.
--
-- Morally equivalent to `UIO (Either e a)`, but implemented without
-- the `Either` for performance.
newtype IOE e a = UnsafeIOE {unIOE :: GHC.IO a}
  deriving (Functor, Applicative, Monad)

{----- Lifted typeclasses -----}

-- | Lift a 'UIO' action to the given monad.
class Monad m => MonadUIO m where
  liftUIO :: UIO a -> m a

instance MonadUIO UIO where
  liftUIO = id
instance MonadUIO (IOE e) where
  liftUIO = UnsafeIOE . unUIO

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
class Monad m => MonadIOE e m | m -> e where
  liftIOE :: IOE e a -> m a

instance MonadIOE e (IOE e) where
  liftIOE = id
instance MonadIOE Void UIO where
  liftIOE = fmap (either absurd id) . toUIO

-- | Provide a function for running an action in the given monad in 'UIO'.
--
-- Must satisfy the idempotency law:
--
-- @
-- withRunInUIO (\\run -> run m) === m
-- @
class MonadUIO m => MonadUnliftUIO m where
  withRunInUIO :: ((forall a. m a -> UIO a) -> UIO b) -> m b

instance MonadUnliftUIO UIO where
  withRunInUIO f = f id
instance MonadUnliftUIO (IOE Void) where
  withRunInUIO f = liftUIO (f liftIOE)

-- | Provide a function for running an action in the given monad in 'IOE'.
--
-- Must satisfy the idempotency law:
--
-- @
-- withRunInIOE (\\run -> (liftIOE . run) m) === m
-- @
class MonadIOE e m => MonadUnliftIOE e m | m -> e where
  withRunInIOE :: ((forall a. m a -> IOE e a) -> IOE e b) -> m b

instance MonadUnliftIOE e (IOE e) where
  withRunInIOE f = f id
instance MonadUnliftIOE Void UIO where
  withRunInIOE f = liftIOE (f liftUIO)

-- | A typeclass for monads that contain an exception that can be converted.
class
  (Exception e1, Exception e2, MonadUnliftIOE e1 m1, MonadUnliftIOE e2 m2) =>
  MonadMapException m1 e1 m2 e2
    | m1 -> e1
    , m2 -> e2
  where
  mapExceptionM :: (e1 -> e2) -> m1 a -> m2 a

instance (Exception e1, Exception e2) => MonadMapException (IOE e1) e1 (IOE e2) e2 where
  mapExceptionM f = fromUIO . fmap (first f) . toUIO

{----- IO convenience type -----}

-- | A helper containing any synchronous exception.
type IO = IOE SomeSyncException

type MonadIO = MonadIOE SomeSyncException
type MonadUnliftIO = MonadUnliftIOE SomeSyncException

-- | 'liftIOE' specialized to 'IO'
liftIO :: MonadIO m => IO a -> m a
liftIO = liftIOE

-- | 'withRunInIOE' specialized to 'IO'
withRunInIO :: MonadUnliftIO m => ((forall a. m a -> IO a) -> IO b) -> m b
withRunInIO = withRunInIOE

{----- IOE operations -----}

-- | Lift an exception to 'SomeSyncException', useful for converting to 'IO'.
--
-- === __Example__
--
-- @
-- foo :: String -> IOE MyException Int
--
-- bar :: IO Int
-- bar = liftE $ foo "hello world"
-- @
liftE ::
  forall e m1 m2 a.
  (MonadMapException m1 e m2 SomeSyncException) =>
  m1 a
  -> m2 a
liftE = mapExceptionM SomeSyncException

throw :: (Exception e, MonadIOE e m) => e -> m a
throw = liftIOE . UnsafeIOE . GHC.throwIO . SomeException SyncExceptionType

throwTo :: (Exception e, MonadUIO m) => ThreadId -> e -> m ()
throwTo tid = liftUIO . UnsafeUIO . GHC.throwTo tid . SomeException AsyncExceptionType

throwImprecise :: Exception e => e -> a
throwImprecise = GHC.throw . SomeException ImpreciseExceptionType

-- | If your handler does not throw an error, consider using 'catchUIO'.
catch ::
  (HasCallStack, Exception e1, Exception e2) =>
  IOE e1 a
  -> (e1 -> IOE e2 a)
  -> IOE e2 a
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

-- TODO: catchAny

{----- Conversions between UIO/IOE -----}

toUIO :: (HasCallStack, Exception e) => IOE e a -> UIO (Either e a)
toUIO m = (Right <$> m) `catchUIO` (pure . Left)

fromUIO :: Exception e => UIO (Either e a) -> IOE e a
fromUIO = fromUIOWith id

fromUIOWith :: Exception e2 => (e1 -> e2) -> UIO (Either e1 a) -> IOE e2 a
fromUIOWith f m = liftUIO m >>= either (throw . f) pure

-- | Convert a 'UIO' action to an 'IO' action.
--
-- Morally equivalent to @liftE . fromUIO@, but more performant.
uioToIO :: Exception e => UIO (Either e a) -> IO a
uioToIO = fromUIOWith SomeSyncException

{----- Interop with unchecked IO -----}

-- | A convenience alias for 'main' functions
type Main = UnsafeIO ()

-- | Convert an unchecked IO action into a checked IO action.
checkIO :: UnsafeIO a -> IO a
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
unsafeCheckIO = mapExceptionM convert . checkIO
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

-- TODO: uncheckIOE, uncheckUIO: evaluate first
-- TODO: reraise AsyncException wrapped in SomeAsyncException

-- | Unchecks an 'IOE' action back into the normal 'GHC.IO' monad.
--
-- Note: 'IOE' wraps all exceptions into 'SomeException', and this function
-- will unwrap them back. However, if you used 'throwImprecise', and the
-- result hasn't been evaluated yet, it'll remain wrapped in 'SomeException'.
uncheckIOE :: IOE e a -> UnsafeIO a
uncheckIOE = GHC.handle go . unIOE
  where
    go (SomeException _ e) =
      case cast e of
        Just (SomeSyncException e') -> GHC.throwIO e'
        _ -> GHC.throwIO e

-- | Unchecks an 'UIO' action back into the normal 'GHC.IO' monad.
--
-- Note: 'UIO' wraps all exceptions into 'SomeException', and this function
-- will unwrap them back. However, if you used 'throwImprecise', and the
-- result hasn't been evaluated yet, it'll remain wrapped in 'SomeException'.
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
data SomeException
  = forall e.
    Exception e =>
    SomeException ExceptionType e

-- TODO: AnyException e

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
