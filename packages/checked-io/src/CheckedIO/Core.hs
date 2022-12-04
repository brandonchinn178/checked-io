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
  throwIO,
  throwTo,
  throwImprecise,
  catch,
  catchAny,
  try,
  mapExceptionM,
  liftE,
  evaluate,

  -- * Cleanup (no recovery)
  bracket,
  bracket_,
  bracketOnError,
  bracketOnError_,
  finally,
  onException,
  withException,

  -- * Masking
  mask,
  uninterruptibleMask,
  mask_,
  uninterruptibleMask_,

  -- * Lifted exception handling
  MonadCatchIO (..),
  tryM,
  bracketM,
  bracketM_,
  bracketOnErrorM,
  bracketOnErrorM_,
  finallyM,
  onExceptionM,
  withExceptionM,
  maskM,
  uninterruptibleMaskM,
  maskM_,
  uninterruptibleMaskM_,

  -- * Interop with unchecked IO
  Main,
  UnsafeIO,
  checkIO,
  checkIOWith,
  checkUIOWith,
  unsafeCheckIO,
  unsafeCheckUIO,
  uncheckIOE,
  uncheckUIO,

  -- * Exceptions in checked IO actions
  AnyException (..),
  SomeSyncException (..),
  fromSyncException,
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

unUIO :: UIO a -> UnsafeIO a
unUIO = unIOE

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

-- | Throw the given exception.
throw :: MonadRunIOE e m => e -> m a
throw = runIOE . UnsafeIOE . GHC.throwIO . AnySyncException . SomeException

-- | Helper equivalent to @liftE . throw@
throwIO :: (Exception e, MonadRunIO m) => e -> m a
throwIO = throw . SomeSyncException

-- | Throw the given exception as an asynchronous exception to the given thread.
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

-- | Handle the exception tracked in 'IOE' if one is thrown.
catch :: forall e1 e2 a. (Exception e1, Exception e2) => IOE e1 a -> (e1 -> IOE e2 a) -> IOE e2 a
catch = catchM

-- | Handle /all/ exceptions in the given 'IOE' action, including non-synchronous
-- exceptions.
--
-- __Warning__: You probably want 'catch' instead; if you catch an
-- async exception, you /must/ be careful to rethrow it after.
--
-- More information:
--
--   * https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell/
--   * https://www.tweag.io/blog/2020-04-16-exceptions-in-haskell/
catchAny :: forall e1 e2 a. (Exception e1, Exception e2) => IOE e1 a -> (AnyException e1 -> IOE e2 a) -> IOE e2 a
catchAny = catchAnyM

-- | Get an @Either@ containing either the result or the exception thrown.
try :: forall e1 e2 a. (Exception e1, Exception e2) => IOE e1 a -> IOE e2 (Either e1 a)
try = tryM

mapExceptionM ::
  forall e1 e2 m1 m2 a.
  (MonadCatchIO e1 m1, MonadRunAsIOE e2 m2) =>
  (e1 -> e2)
  -> m1 a
  -> m2 a
mapExceptionM f m = m `catchM` (throw . f)

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

{----- Cleanup (no recovery) -----}

-- | Allocate and clean up a resource safely.
--
-- Runs the clean up in an uninterruptible mask. For more information:
--
--   * https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Exception.html#v:bracket
--   * https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Exception-Base.html#v:bracket
bracket :: Exception e => IOE e a -> (a -> IOE e b) -> (a -> IOE e c) -> IOE e c
bracket = bracketM

-- | Same as 'bracket', except without passing the result of the acquire action to the
-- release or run actions.
bracket_ :: Exception e => IOE e a -> IOE e b -> IOE e c -> IOE e c
bracket_ = bracketM_

-- | Same as 'bracket', but only perform the cleanup if an exception is thrown.
bracketOnError ::
  (Exception e, Exception e') =>
  IOE e a
  -> (a -> IOE e' b)
  -> (a -> IOE e c)
  -> IOE e c
bracketOnError = bracketOnErrorM

-- | Same as 'bracket_', but only perform the cleanup if an exception is thrown.
bracketOnError_ ::
  (Exception e, Exception e') =>
  IOE e a
  -> IOE e' b
  -> IOE e c
  -> IOE e c
bracketOnError_ = bracketOnErrorM_

-- | A specialized variant of 'bracket' that just runs a computation afterward.
finally :: Exception e => IOE e a -> IOE e b -> IOE e a
finally = finallyM

-- | Like 'finally', except only runs the cleanup if an exception occurs.
onException :: (Exception e, Exception e') => IOE e a -> IOE e' b -> IOE e a
onException = onExceptionM

-- | Like 'onException', except passing the exception to the cleanup function.
--
-- Unlike 'catchAny', you /don't/ need to worry about rethrowing async exceptions
-- because 'withException' always rethrows the exception after running the cleanup.
withException :: (Exception e, Exception e') => IOE e a -> (AnyException e -> IOE e' b) -> IOE e a
withException = withExceptionM

{----- Masking -----}

-- | Execute an IO action with asynchronous exceptions masked.
--
-- https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-IO.html#v:mask
mask ::
  Exception e =>
  ((forall e' a. Exception e' => IOE e' a -> IOE e' a) -> IOE e b)
  -> IOE e b
mask = maskM

-- | Like 'mask', but the masked computation is not interruptible.
--
-- __Warning__: Use with great care, as a thread running with 'uninterruptibleMask' will
-- be unresponsive and unkillable if it blocks at all.
--
-- https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-IO.html#v:uninterruptibleMask
uninterruptibleMask ::
  Exception e =>
  ((forall e' a. Exception e' => IOE e' a -> IOE e' a) -> IOE e b)
  -> IOE e b
uninterruptibleMask = uninterruptibleMaskM

-- | Like 'mask', but does not pass a @restore@ action to the argument.
mask_ :: Exception e => IOE e a -> IOE e a
mask_ = maskM_

-- | Like 'uninterruptibleMask', but does not pass a @restore@ action to the argument.
uninterruptibleMask_ :: Exception e => IOE e a -> IOE e a
uninterruptibleMask_ = uninterruptibleMaskM_

{----- Lifted exception handling -----}

class (MonadRunAsIOE e m, Exception e) => MonadCatchIO e m | m -> e where
  {-# MINIMAL catchAnyM #-}

  -- | 'catch' generalized to any 'MonadCatchIO' + 'MonadRunAsIOE'
  catchM :: MonadRunAsIOE e' m' => m a -> (e -> m' a) -> m' a
  catchM m f =
    m `catchAnyM` \case
      AnySyncException e -> f e
      AnyAsyncException e -> rethrow (AnyAsyncException e)
      AnyImpreciseException e -> rethrow (AnyImpreciseException e)

  -- | 'catchAny' generalized to any 'MonadCatchIO' + 'MonadRunAsIOE'
  catchAnyM :: MonadRunAsIOE e' m' => m a -> (AnyException e -> m' a) -> m' a

instance Exception e => MonadCatchIO e (IOE e) where
  catchAnyM (UnsafeIOE m) f =
    withRunAsIOE $ \run ->
      UnsafeIOE $ m `GHC.catch` (unIOE . run . f . castAnyException)

-- | 'try' generalized to any 'MonadCatchIO' + 'MonadRunIOE'
tryM :: (MonadCatchIO e m1, MonadRunIOE e' m2) => m1 a -> m2 (Either e a)
tryM m = runIOE $ (Right <$> m) `catchM` (pure . Left)

bracketM' ::
  (MonadCatchIO e m, MonadCatchIO e' m') =>
  m a -- ^ acquire
  -> (a -> m b1) -- ^ release on success
  -> (AnyException e -> a -> m' b2) -- ^ release on error
  -> (a -> m c) -- ^ action
  -> m c
bracketM' acquire releaseOnSuccess releaseOnError action =
  maskM $ \restore -> do
    x <- acquire
    tryAny (restore $ action x) >>= \case
      Right y -> do
        _ <- uninterruptibleMaskM_ (releaseOnSuccess x)
        pure y
      Left e -> do
        -- explicitly ignore synchronous exceptions
        _ <- tryM $ uninterruptibleMaskM_ (releaseOnError e x)
        rethrow e
  where
    tryAny m = (Right <$> m) `catchAnyM` (pure . Left)

-- | 'bracket' generalized to any 'MonadCatchIO'
bracketM ::
  MonadCatchIO e m =>
  m a
  -> (a -> m b)
  -> (a -> m c)
  -> m c
bracketM acquire release action = bracketM' acquire release (\_ -> release) action

-- | 'bracket_' generalized to any 'MonadCatchIO'
bracketM_ :: MonadCatchIO e m => m a -> m b -> m c -> m c
bracketM_ acquire release action = bracketM acquire (const release) (const action)

-- | 'bracketOnError' generalized to any 'MonadCatchIO'
bracketOnErrorM ::
  (MonadCatchIO e m, MonadCatchIO e' m') =>
  m a
  -> (a -> m' b)
  -> (a -> m c)
  -> m c
bracketOnErrorM acquire release action = bracketM' acquire (\_ -> pure ()) (\_ -> release) action

-- | 'bracketOnError_' generalized to any 'MonadCatchIO'
bracketOnErrorM_ ::
  (MonadCatchIO e m, MonadCatchIO e' m') =>
  m a
  -> m' b
  -> m c
  -> m c
bracketOnErrorM_ acquire releaseOnError action = bracketOnErrorM acquire (const releaseOnError) (const action)

-- | 'finally' generalized to any 'MonadCatchIO'
finallyM :: MonadCatchIO e m => m a -> m b -> m a
finallyM action cleanup = bracketM_ (pure ()) cleanup action

-- | 'onException' generalized to any 'MonadCatchIO'
onExceptionM :: (MonadCatchIO e m, MonadCatchIO e' m') => m a -> m' b -> m a
onExceptionM action after = withExceptionM action (\_ -> after)

-- | 'withException' generalized to any 'MonadCatchIO'
withExceptionM :: (MonadCatchIO e m, MonadCatchIO e' m') => m a -> (AnyException e -> m' b) -> m a
withExceptionM action after =
  bracketM'
    (pure ())
    (\_ -> pure ())
    (\e _ -> after e)
    (\_ -> action)

maskM' ::
  MonadCatchIO e m =>
  (forall b'. ((forall a'. UnsafeIO a' -> UnsafeIO a') -> UnsafeIO b') -> UnsafeIO b')
  -> ((forall m' e' a. MonadCatchIO e' m' => m' a -> m' a) -> m b)
  -> m b
maskM' unsafeMask f =
  withRunAsIOE $ \run ->
    fromUIO $ maskIOE $ \restore -> do
      let restoreIOE m =
            fromUIO $
              checkIOWith
                (\e -> error $ "restore unexpectedly threw an error: " ++ show e)
                (restore $ unUIO (tryM m))
      unUIO $ try $ run $ f restoreIOE
  where
    maskIOE :: MonadRunIOE e m => ((forall a. UnsafeIO a -> UnsafeIO a) -> UnsafeIO b) -> m b
    maskIOE f' = checkIOWith (\e -> error $ "mask unexpectedly threw an error: " ++ show e) (unsafeMask f')

-- | 'mask' generalized to any 'MonadCatchIO'
maskM ::
  MonadCatchIO e m =>
  ((forall m' e' a. MonadCatchIO e' m' => m' a -> m' a) -> m b)
  -> m b
maskM = maskM' GHC.mask

-- | 'uninterruptibleMask' generalized to any 'MonadCatchIO'
uninterruptibleMaskM ::
  MonadCatchIO e m =>
  ((forall m' e' a. MonadCatchIO e' m' => m' a -> m' a) -> m b)
  -> m b
uninterruptibleMaskM = maskM' GHC.uninterruptibleMask

-- | 'mask_' generalized to any 'MonadCatchIO'
maskM_ :: MonadCatchIO e m => m a -> m a
maskM_ action = maskM (\_ -> action)

-- | 'uninterruptibleMask_' generalized to any 'MonadCatchIO'
uninterruptibleMaskM_ :: MonadCatchIO e m => m a -> m a
uninterruptibleMaskM_ action = uninterruptibleMaskM (\_ -> action)

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

-- | 'uncheckIOE' specialized to UIO.
uncheckUIO :: UIO a -> UnsafeIO a
uncheckUIO = uncheckIOE

-- | Internal-only function to rethrow an 'AnyException' caught by 'catchAnyM'.
rethrow :: MonadRunIOE e m => AnyException e -> m a
rethrow = runIOE . UnsafeIOE . GHC.throwIO . fmap SomeException

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

-- | Check a specific exception type in 'SomeSyncException'
fromSyncException :: Exception e => SomeSyncException -> Maybe e
fromSyncException (SomeSyncException e) = fromException (SomeException e)
