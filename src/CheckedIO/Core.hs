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

  -- * Lifting UIO
  fromUIO,
  fromUIOWith,
  uioToIO,

  -- * IOE exception handling
  throw,
  throwTo,
  throwImprecise,
  MonadCatchIO (..),
  try,
  mapExceptionM,
  liftE,

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
import Data.Void (Void)
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
--   liftIOE = ExceptT . try
-- @
class MonadUIO m => MonadIOE e m | m -> e where
  liftIOE :: IOE e a -> m a

instance MonadIOE e (IOE e) where
  liftIOE = id
instance MonadIOE Void UIO where
  -- trust that IOE doesn't have a Void exception floating around.
  -- should be equivalent to `fmap (either absurd id) . try`, but
  -- be more performant
  liftIOE = UnsafeUIO . unIOE

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

{----- Lifting UIO -----}

-- | Convert @UIO (Either e a)@ to @IOE e a@ (or any other monad
-- with a @MonadIOE@ instance).
--
-- > fromUIO m = liftUIO m >>= either throw pure
fromUIO ::
  (Exception e, MonadIOE e m) =>
  UIO (Either e a)
  -> m a
fromUIO = fromUIOWith id

-- | Convert @UIO (Either e1 a)@ to @IOE e2 a@ (or any other monad
-- with a @MonadIOE@ instance) with the given
-- transformation function.
--
-- Same as @mapExceptionM f . fromUIO@, but more performant.
fromUIOWith ::
  (Exception e2, MonadIOE e2 m) =>
  (e1 -> e2)
  -> UIO (Either e1 a)
  -> m a
fromUIOWith f m = liftUIO m >>= either (throw . f) pure

-- | Convert @UIO (Either e a)@ action to @IO a@ (or any other monad
-- with a @MonadIO@ instance).
--
-- Same as @liftE . fromUIO@, but more performant.
uioToIO :: (Exception e, MonadIO m) => UIO (Either e a) -> m a
uioToIO = fromUIOWith SomeSyncException

{----- IOE exception handling -----}

throw :: (Exception e, MonadIOE e m) => e -> m a
throw = liftIOE . UnsafeIOE . GHC.throwIO . SomeException SyncExceptionType

throwTo :: (Exception e, MonadUIO m) => ThreadId -> e -> m ()
throwTo tid = liftUIO . UnsafeUIO . GHC.throwTo tid . SomeException AsyncExceptionType

throwImprecise :: Exception e => e -> a
throwImprecise = GHC.throw . SomeException ImpreciseExceptionType

-- TODO: catchAny (-> IOE Void), define catch + catchE with catchAny
class (MonadIOE e ioe, Exception e) => MonadCatchIO e ioe | ioe -> e where
  catch :: MonadUnliftUIO uio => ioe a -> (e -> uio a) -> uio a

  -- | Same as 'catch', except allows throwing exceptions in the handler
  catchE :: MonadUnliftIOE e' ioe' => ioe a -> (e -> ioe' a) -> ioe' a

instance Exception e => MonadCatchIO e (IOE e) where
  catch m f =
    withRunInUIO $ \run ->
      liftIOE $ m `catchE` (liftUIO @(IOE Void) . run . f)

  catchE (UnsafeIOE m) f =
    withRunInIOE $ \run ->
      UnsafeIOE $
        m `GHC.catch` \case
          SomeException SyncExceptionType e ->
            case cast e of
              Just e' -> unIOE (run $ f e')
              Nothing ->
                error $
                  "checked-io invariant violation: IOE contained an unexpected synchronous exception: "
                    ++ show e
          e -> GHC.throwIO e

-- | Convert @IOE e a@ to @UIO (Either e a)@
try :: (MonadCatchIO e ioe, MonadUIO uio) => ioe a -> uio (Either e a)
try m = liftUIO $ (Right <$> m) `catch` (pure . Left)

-- TODO: withException
-- TODO: bracket
-- TODO: mask

mapExceptionM ::
  forall e1 e2 m1 m2 a.
  (MonadCatchIO e1 m1, MonadCatchIO e2 m2) =>
  (e1 -> e2)
  -> m1 a
  -> m2 a
mapExceptionM f = fromUIO . fmap (first f) . try

-- | Lift an exception to 'SomeSyncException', useful for converting to 'IO'.
--
-- > liftE = mapExceptionM SomeSyncException
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
  (MonadCatchIO e m1, MonadCatchIO SomeSyncException m2) =>
  m1 a
  -> m2 a
liftE = mapExceptionM SomeSyncException


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
unsafeCheckUIO = fmap (either foundError id) . try . checkIO
  where
    foundError (SomeSyncException e) =
      withFrozenCallStack . error $
        "unsafeCheckUIO was called on an action that unexpectedly threw an error: "
          ++ show e

-- | Unchecks an 'IOE' action back into the normal 'GHC.IO' monad.
--
-- Note: 'IOE' wraps all exceptions into 'SomeException', and this function
-- will unwrap them back. However, if you used 'throwImprecise', and the
-- result hasn't been evaluated yet, it'll remain wrapped in 'SomeException'.
uncheckIOE :: IOE e a -> UnsafeIO a
uncheckIOE m =
  (unIOE m >>= GHC.evaluate) `GHC.catch` \case
    SomeException SyncExceptionType e | Just (SomeSyncException e') <- cast e -> GHC.throwIO e'
    SomeException AsyncExceptionType e -> GHC.throwIO $ GHC.SomeAsyncException e
    SomeException _ e -> GHC.throwIO e

-- | Unchecks an 'UIO' action back into the normal 'GHC.IO' monad.
--
-- Note: 'UIO' wraps all exceptions into 'SomeException', and this function
-- will unwrap them back. However, if you used 'throwImprecise', and the
-- result hasn't been evaluated yet, it'll remain wrapped in 'SomeException'.
uncheckUIO :: UIO a -> UnsafeIO a
uncheckUIO = uncheckIOE . liftUIO

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
