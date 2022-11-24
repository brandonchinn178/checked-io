{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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

  -- * UIO convenience type
  UIO,
  MonadRunUIO,
  MonadRunAsUIO,
  runUIO,
  withRunAsUIO,
  fromUIO,
  fromUIOWith,
  uioToIO,

  -- * Exception handling
  throw,
  throwTo,
  throwImprecise,
  MonadCatchIO (..),
  try,
  mapExceptionM,
  liftE,
  evaluate,

  -- * Exception handling specialized to IOE
  throwIOE,
  throwToIOE,
  catchIOE,
  catchAnyIOE,
  tryIOE,

  -- * Interop with unchecked IO
  Main,
  UnsafeIO,
  checkIO,
  checkIOWith,
  checkUIOWith,
  unsafeCheckIO,
  unsafeCheckUIO,
  uncheckIOE,

  -- * Exceptions in checked IO actions
  AnyException (..),
  SomeSyncException (..),
) where

import Control.Concurrent (ThreadId)
import Control.Exception (Exception (..), SomeException (..))
import qualified Control.Exception as GHC
import qualified Control.Exception.Base as GHC
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

-- | A checked IO action that can only throw synchronous exceptions
-- of the given type.
--
-- Morally equivalent to @UIO (Either e a)@, but implemented without
-- the `Either` for performance.
newtype IOE e a = UnsafeIOE {unIOE :: UnsafeIO a}
  deriving (Functor, Applicative, Monad)

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
class (Monad m, Exception e) => MonadRunIOE e m | m -> e where
  runIOE :: IOE e a -> m a

instance Exception e => MonadRunIOE e (IOE e) where
  runIOE = id

-- | Provide a function for running an action in the given monad in 'IOE'.
--
-- Instances must satisfy the following laws:
--
-- [Identity] @withRunAsIOE (\\run -> run m) === m@
-- [Inverse]  @withRunAsIOE (\\_ -> m) === runIOE m@
class MonadRunIOE e m => MonadRunAsIOE e m | m -> e where
  withRunAsIOE :: ((forall a. m a -> IOE e a) -> IOE e b) -> m b

instance Exception e => MonadRunAsIOE e (IOE e) where
  withRunAsIOE f = f id

{----- IO convenience type -----}

-- | A helper containing any synchronous exception.
type IO = IOE SomeSyncException

-- | 'MonadRunIOE' specialized to 'IO'
type MonadRunIO = MonadRunIOE SomeSyncException

-- | 'MonadRunAsIOE' specialized to 'IO'
type MonadRunAsIO = MonadRunAsIOE SomeSyncException

-- | 'runIOE' specialized to 'IO'
runIO :: MonadRunIO m => IO a -> m a
runIO = runIOE

-- | 'withRunAsIOE' specialized to 'IO'
withRunAsIO :: MonadRunAsIO m => ((forall a. m a -> IO a) -> IO b) -> m b
withRunAsIO = withRunAsIOE

{----- UIO convenience type -----}

-- | A checked IO action that cannot throw /any/ (synchronous) exceptions.
type UIO = IOE Void

type MonadRunUIO = MonadRunIOE Void

type MonadRunAsUIO = MonadRunAsIOE Void

runUIO :: MonadRunIOE e m => UIO a -> m a
runUIO = runIOE . castIOE
  where
    -- assuming that IOE doesn't have a Void exception floating around, this
    -- should be equivalent to `fmap (either absurd id) . try`, but this is
    -- more performant, as it doesn't have to catch async or imprecise
    -- exceptions
    castIOE = UnsafeIOE . unIOE

withRunAsUIO :: MonadRunAsUIO m => ((forall a. m a -> UIO a) -> UIO b) -> m b
withRunAsUIO = withRunAsIOE

-- | Convert @UIO (Either e a)@ to @IOE e a@ (or any other monad
-- with a @MonadRunIOE@ instance).
--
-- > fromUIO m = runUIO m >>= either throw pure
fromUIO ::
  forall e m a.
  MonadRunIOE e m =>
  UIO (Either e a)
  -> m a
fromUIO = fromUIOWith id

-- | Convert @UIO (Either e1 a)@ to @IOE e2 a@ (or any other monad
-- with a @MonadRunIOE@ instance) with the given
-- transformation function.
--
-- Same as @mapExceptionM f . fromUIO@, but more performant.
fromUIOWith ::
  forall e1 e2 m a.
  MonadRunIOE e2 m =>
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

{----- Exception handling -----}

-- | 'throwIOE' generalized to any 'MonadRunIOE'
throw :: MonadRunIOE e m => e -> m a
throw = runIOE . UnsafeIOE . GHC.throwIO . AnySyncException . SomeException

-- | 'throwToIOE' generalized to any 'MonadRunIOE'
throwTo :: (Exception e, MonadRunIOE e' m) => ThreadId -> e -> m ()
throwTo tid = runIOE . UnsafeIOE . GHC.throwTo tid . AnyAsyncException @SomeException . SomeException

-- | Throw an imprecise exception in a pure context.
--
-- __Warning__: Because the exception isn't tracked at the type level, this
-- should be reserved for unrecoverable errors that aren't expected to be
-- handled. Also, if this value is not evaluated before 'uncheckIOE'  is
-- called, it'll remain wrapped in 'AnyException'.
throwImprecise :: Exception e => e -> a
throwImprecise = GHC.throw . AnyImpreciseException @SomeException . SomeException

class (MonadRunIOE e m, Exception e) => MonadCatchIO e m | m -> e where
  {-# MINIMAL catchAny #-}

  -- | 'catchIOE' generalized to any 'MonadCatchIO' + 'MonadRunAsIOE'
  catch :: MonadRunAsIOE e' m' => m a -> (e -> m' a) -> m' a
  catch m f =
    m `catchAny` \case
      AnySyncException e -> f e
      e -> runIOE . UnsafeIOE . GHC.throwIO $ SomeException <$> e

  -- | 'catchAnyIOE' generalized to any 'MonadCatchIO' + 'MonadRunAsIOE'
  catchAny :: MonadRunAsIOE e' m' => m a -> (AnyException e -> m' a) -> m' a

instance Exception e => MonadCatchIO e (IOE e) where
  catchAny (UnsafeIOE m) f =
    withRunAsIOE $ \run ->
      UnsafeIOE $ m `GHC.catch` (unIOE . run . f . castAnyException)

-- | 'tryIOE' generalized to any 'MonadCatchIO' + 'MonadRunIOE'
try :: (MonadCatchIO e m1, MonadRunIOE e' m2) => m1 a -> m2 (Either e a)
try m = runIOE $ (Right <$> m) `catch` (pure . Left)

-- TODO: withException
-- TODO: bracket
-- TODO: mask

mapExceptionM ::
  forall e1 e2 m1 m2 a.
  (MonadCatchIO e1 m1, MonadRunAsIOE e2 m2) =>
  (e1 -> e2)
  -> m1 a
  -> m2 a
mapExceptionM f m = m `catch` (throw . f)

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
  (MonadCatchIO e m1, MonadRunAsIO m2) =>
  m1 a
  -> m2 a
liftE = mapExceptionM SomeSyncException

evaluate :: MonadRunIOE e m => a -> m a
evaluate = runIOE . UnsafeIOE . GHC.evaluate

{----- Exception handling specialized to IOE -----}

-- | Throw the given exception in 'IOE'.
throwIOE :: forall e a. Exception e => e -> IOE e a
throwIOE = throw

-- | Throw the given exception as an asynchronous exception to the given thread.
throwToIOE :: forall e. Exception e => ThreadId -> e -> IOE e ()
throwToIOE = throwTo

-- | Handle the exception tracked in 'IOE' if one is thrown.
catchIOE :: forall e1 e2 a. (Exception e1, Exception e2) => IOE e1 a -> (e1 -> IOE e2 a) -> IOE e2 a
catchIOE = catch

-- | Handle /all/ exceptions in the given 'IOE' action, including non-synchronous
-- exceptions.
--
-- __Warning__: You probably want 'catchIOE' instead; if you catch an
-- async exception, you /must/ be careful to rethrow it after.
--
-- More information:
--
--   * https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell/
--   * https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell/
catchAnyIOE :: forall e1 e2 a. (Exception e1, Exception e2) => IOE e1 a -> (AnyException e1 -> IOE e2 a) -> IOE e2 a
catchAnyIOE = catchAny

-- | Get an @Either@ containing either the result or the exception thrown.
tryIOE :: forall e1 e2 a. (Exception e1, Exception e2) => IOE e1 a -> IOE e2 (Either e1 a)
tryIOE = try

{----- Interop with unchecked IO -----}

-- | A convenient alias for 'main' functions
type Main = UnsafeIO ()

-- | Convert an unchecked IO action into a checked IO action.
checkIO :: MonadRunIO m => UnsafeIO a -> m a
checkIO = checkIOWith (\(SomeException e) -> SomeSyncException e)

-- | Same as 'checkIO' except converting a synchronous exception with the given function.
--
-- Equivalent to @mapExceptionM f . checkIO@, except more performant.
checkIOWith :: (MonadRunIOE e m) => (SomeException -> e) -> UnsafeIO a -> m a
checkIOWith f m = checkUIOWith f m >>= either throw pure

-- | Same as 'checkIOWith' except returning the exception as an @Either@ instead of
-- throwing it.
checkUIOWith :: MonadRunIOE e' m => (SomeException -> e) -> UnsafeIO a -> m (Either e a)
checkUIOWith f m =
  runIOE . UnsafeIOE $
    GHC.try m >>= \case
      Right x -> pure $ Right x
      Left (AnySyncException e) -> pure $ Left (f e)
      Left e -> GHC.throwIO e

-- | Convert an unchecked IO action into a checked IO action that can throw
-- the given exception type.
--
-- __Warning__: If the IO action threw a different synchronous exception,
-- this function will error. Prefer using 'checkIOWith' and calling 'error'
-- yourself with a better error message.
unsafeCheckIO :: (HasCallStack, MonadRunIOE e m) => UnsafeIO a -> m a
unsafeCheckIO = runIOE . mapExceptionM convert . checkIO @IO
  where
    convert (SomeSyncException e) =
      case cast e of
        Just e' -> e'
        Nothing ->
          withFrozenCallStack . error $
            "unsafeCheckIO was called on an action that threw an unexpected error: "
              ++ show e

-- | Same as 'unsafeCheckIO', except expects /no/ exceptions to be thrown.
unsafeCheckUIO :: (HasCallStack, MonadRunIOE e m) => UnsafeIO a -> m a
unsafeCheckUIO = withFrozenCallStack . runUIO . unsafeCheckIO

-- | Unchecks an 'IOE' action back into the normal 'GHC.IO' monad.
--
-- __Warning__: Imprecise exceptions might be wrapped in 'AnyException'
-- after converting to 'UnsafeIO'. See 'throwImprecise' for more details.
uncheckIOE :: forall e a. Exception e => IOE e a -> UnsafeIO a
uncheckIOE m =
  let UnsafeIOE m' = (Right <$> m >>= evaluate) `catchAny` (pure @UIO . Left)
  in  m' >>= \case
        Right a -> pure a
        Left (AnySyncException e) ->
          case cast e of
            Just (SomeSyncException e') -> GHC.throwIO e'
            Nothing -> GHC.throwIO e
        Left (AnyAsyncException (SomeException e)) -> GHC.throwIO (GHC.SomeAsyncException e)
        Left (AnyImpreciseException e) -> GHC.throwIO e

{----- Exceptions in IOE -----}

-- | All exceptions floating around 'IOE' (synchronous
-- or otherwise) will be an @AnyException SomeException@.
data AnyException e
  = AnySyncException e
  | AnyAsyncException SomeException
  | AnyImpreciseException SomeException
  -- TODO: should ExitCode be handled specially?
  deriving (Functor)

deriving instance Show e => Show (AnyException e)

instance Exception (AnyException SomeException) where
  fromException = Just . toAnyException

toAnyException :: SomeException -> AnyException SomeException
toAnyException someE@(SomeException e) =
  fromMaybe (asSync someE) . asum $
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
    asSync :: SomeException -> AnyException SomeException
    asSync = AnySyncException

    asAsync :: Exception e => e -> AnyException SomeException
    asAsync = AnyAsyncException . SomeException

    asImprecise :: Exception e => e -> AnyException SomeException
    asImprecise = AnyImpreciseException . SomeException

    castE :: forall e. Exception e => Maybe e
    castE = fromException someE

castAnyException :: forall e. Exception e => AnyException SomeException -> AnyException e
castAnyException = fmap castExpected
  where
    castExpected (SomeException eActual) =
      case cast eActual of
        Just e -> e
        Nothing ->
          errorWithoutStackTrace . unwords $
            [ "checked-io invariant violation:"
            , "IOE contained an unexpected synchronous exception:"
            , "Expected `" ++ show (typeRep $ Proxy @e) ++ "`,"
            , "got `" ++ show (typeOf eActual) ++ "`."
            , show eActual
            ]

data SomeSyncException = forall e. Exception e => SomeSyncException e

instance Show SomeSyncException where
  showsPrec p (SomeSyncException e) = showsPrec p e

instance Exception SomeSyncException
