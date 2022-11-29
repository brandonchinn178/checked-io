module CheckedIO.IORef (
  IORef,
  newIORef,
  readIORef,
  writeIORef,
  modifyIORef,
  modifyIORef',
  atomicModifyIORef,
  atomicModifyIORef',
  atomicWriteIORef,
) where

import Data.IORef (IORef)
import qualified Data.IORef as IORef

import CheckedIO.Core (MonadRunIOE, checkIOWith)
import CheckedIO.Prelude

newIORef :: MonadRunIOE e m => a -> m (IORef a)
newIORef a = checkIOWith check $ IORef.newIORef a
  where
    check e = error $ "newIORef unexpectedly threw an error: " ++ show e

readIORef :: MonadRunIOE e m => IORef a -> m a
readIORef ref = checkIOWith check $ IORef.readIORef ref
  where
    check e = error $ "readIORef unexpectedly threw an error: " ++ show e

writeIORef :: MonadRunIOE e m => IORef a -> a -> m ()
writeIORef ref a = checkIOWith check $ IORef.writeIORef ref a
  where
    check e = error $ "writeIORef unexpectedly threw an error: " ++ show e

modifyIORef :: MonadRunIOE e m => IORef a -> (a -> a) -> m ()
modifyIORef ref f = checkIOWith check $ IORef.modifyIORef ref f
  where
    check e = error $ "modifyIORef unexpectedly threw an error: " ++ show e

modifyIORef' :: MonadRunIOE e m => IORef a -> (a -> a) -> m ()
modifyIORef' ref f = checkIOWith check $ IORef.modifyIORef' ref f
  where
    check e = error $ "modifyIORef' unexpectedly threw an error: " ++ show e

atomicModifyIORef :: MonadRunIOE e m => IORef a -> (a -> (a, b)) -> m b
atomicModifyIORef ref f = checkIOWith check $ IORef.atomicModifyIORef ref f
  where
    check e = error $ "atomicModifyIORef unexpectedly threw an error: " ++ show e

atomicModifyIORef' :: MonadRunIOE e m => IORef a -> (a -> (a, b)) -> m b
atomicModifyIORef' ref f = checkIOWith check $ IORef.atomicModifyIORef' ref f
  where
    check e = error $ "atomicModifyIORef' unexpectedly threw an error: " ++ show e

atomicWriteIORef :: MonadRunIOE e m => IORef a -> a -> m ()
atomicWriteIORef ref a = checkIOWith check $ IORef.atomicWriteIORef ref a
  where
    check e = error $ "atomicWriteIORef unexpectedly threw an error: " ++ show e

-- TODO: mkWeakIORef
