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
  MonadRunUIO (..),
  MonadRunAsUIO (..),

  -- * IOE: Checked IO with exceptions
  IOE (UnsafeIOE),
  MonadRunIOE (..),
  MonadRunAsIOE (..),

  -- * IO convenience type
  IO,
  MonadRunIO,
  MonadRunAsIO,
  runIO,
  withRunAsIO,

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
import Data.Proxy (Proxy (..))
import Data.Typeable (cast, typeOf, typeRep)
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

-- | Run a 'UIO' action in the given monad.
class Monad m => MonadRunUIO m where
  runUIO :: UIO a -> m a

instance MonadRunUIO UIO where
  runUIO = id
instance MonadRunUIO (IOE e) where
  runUIO = UnsafeIOE . unUIO

-- | Run an 'IOE' action in the given monad.
--
-- e.g. to convert an 'IOE' action to an 'ExceptT' stack on top of 'UIO':
--
-- @
-- newtype ExceptT e m a = ExceptT { unExceptT :: m (Either e a) }
--   deriving (Functor, Applicative, Monad)
--
-- instance MonadRunUIO m => MonadRunIOE e (ExceptT e m) where
--   runIOE = ExceptT . try
-- @
class MonadRunUIO m => MonadRunIOE e m | m -> e where
  runIOE :: IOE e a -> m a

instance MonadRunIOE e (IOE e) where
  runIOE = id
instance MonadRunIOE Void UIO where
  -- trust that IOE doesn't have a Void exception floating around.
  -- should be equivalent to `fmap (either absurd id) . try`, but
  -- be more performant
  runIOE = UnsafeUIO . unIOE

-- | Provide a function for running an action in the given monad in 'UIO'.
--
-- Instances must satisfy the following laws:
--
-- [Identity] @withRunAsUIO (\\run -> run m) === m@
-- [Inverse]  @withRunAsUIO (\\_ -> m) === runUIO m@
class MonadRunAsIOE Void m => MonadRunAsUIO m where
  withRunAsUIO :: ((forall a. m a -> UIO a) -> UIO b) -> m b

instance MonadRunAsUIO UIO where
  withRunAsUIO f = f id
instance MonadRunAsUIO (IOE Void) where
  withRunAsUIO f = runUIO (f runIOE)

-- | Provide a function for running an action in the given monad in 'IOE'.
--
-- Must satisfy the idempotency law:
--
-- @
-- withRunAsIOE (\\run -> (runIOE . run) m) === m
-- @
class MonadRunIOE e m => MonadRunAsIOE e m | m -> e where
  withRunAsIOE :: ((forall a. m a -> IOE e a) -> IOE e b) -> m b

instance MonadRunAsIOE e (IOE e) where
  withRunAsIOE f = f id
instance MonadRunAsIOE Void UIO where
  withRunAsIOE f = runIOE (f runUIO)

{----- IO convenience type -----}

-- | A helper containing any synchronous exception.
type IO = IOE SomeSyncException

type MonadRunIO = MonadRunIOE SomeSyncException
type MonadRunAsIO = MonadRunAsIOE SomeSyncException

-- | 'runIOE' specialized to 'IO'
runIO :: MonadRunIO m => IO a -> m a
runIO = runIOE

-- | 'withRunAsIOE' specialized to 'IO'
withRunAsIO :: MonadRunAsIO m => ((forall a. m a -> IO a) -> IO b) -> m b
withRunAsIO = withRunAsIOE

{----- Lifting UIO -----}

-- | Convert @UIO (Either e a)@ to @IOE e a@ (or any other monad
-- with a @MonadRunIOE@ instance).
--
-- > fromUIO m = runUIO m >>= either throw pure
fromUIO ::
  (Exception e, MonadRunIOE e m) =>
  UIO (Either e a)
  -> m a
fromUIO = fromUIOWith id

-- | Convert @UIO (Either e1 a)@ to @IOE e2 a@ (or any other monad
-- with a @MonadRunIOE@ instance) with the given
-- transformation function.
--
-- Same as @mapExceptionM f . fromUIO@, but more performant.
fromUIOWith ::
  (Exception e2, MonadRunIOE e2 m) =>
  (e1 -> e2)
  -> UIO (Either e1 a)
  -> m a
fromUIOWith f m = runUIO m >>= either (throw . f) pure

-- | Convert @UIO (Either e a)@ action to @IO a@ (or any other monad
-- with a @MonadRunIO@ instance).
--
-- Same as @liftE . fromUIO@, but more performant.
uioToIO :: (Exception e, MonadRunIO m) => UIO (Either e a) -> m a
uioToIO = fromUIOWith SomeSyncException

{----- IOE exception handling -----}

throw :: (Exception e, MonadRunIOE e m) => e -> m a
throw = runIOE . UnsafeIOE . GHC.throwIO . SomeException SyncExceptionType

throwTo :: (Exception e, MonadRunUIO m) => ThreadId -> e -> m ()
throwTo tid = runUIO . UnsafeUIO . GHC.throwTo tid . SomeException AsyncExceptionType

throwImprecise :: Exception e => e -> a
throwImprecise = GHC.throw . SomeException ImpreciseExceptionType

class (MonadRunIOE e ioe, Exception e) => MonadCatchIO e ioe | ioe -> e where
  {-# MINIMAL catchAny #-}

  catch :: MonadRunAsUIO uio => ioe a -> (e -> uio a) -> uio a
  catch m f =
    withRunAsUIO $ \run ->
      runIOE $ m `catchE` (runUIO @(IOE Void) . run . f)

  -- | Same as 'catch', except allows throwing exceptions in the handler
  catchE :: MonadRunAsIOE e' ioe' => ioe a -> (e -> ioe' a) -> ioe' a
  catchE m f =
    m `catchAny` \case
      Right e -> f e
      Left e -> runIOE . UnsafeIOE . GHC.throwIO $ e

  catchAny :: MonadRunAsIOE e' ioe' => ioe a -> (Either SomeException e -> ioe' a) -> ioe' a

instance Exception e => MonadCatchIO e (IOE e) where
  catchAny (UnsafeIOE m) f =
    withRunAsIOE $ \run ->
      UnsafeIOE $ m `GHC.catch` (unIOE . run . f . fromSomeException)
    where
      fromSomeException = \case
        SomeException SyncExceptionType e -> Right $ castExpected e
        e -> Left e

      castExpected e =
        case cast e of
          Just e' -> e'
          Nothing ->
            errorWithoutStackTrace . unwords $
              [ "checked-io invariant violation:"
              , "IOE contained an unexpected synchronous exception:"
              , "Expected `" ++ show (typeRep $ Proxy @e) ++ "`,"
              , "got `" ++ show (typeOf e) ++ "`."
              , show e
              ]

instance MonadCatchIO Void UIO where
  catchAny = catchAny . runUIO @(IOE Void)

-- | Convert @IOE e a@ to @UIO (Either e a)@
try :: (MonadCatchIO e ioe, MonadRunUIO uio) => ioe a -> uio (Either e a)
try m = runUIO $ (Right <$> m) `catch` (pure . Left)

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
uncheckUIO = uncheckIOE . runUIO

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
