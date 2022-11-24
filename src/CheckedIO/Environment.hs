{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- | A checked version of @System.Environment@
module CheckedIO.Environment (
  -- * getEnv
  GetEnvVarError (..),
  getEnvIO,
  getEnv,
  getEnvUIO,

  -- * lookupEnv
  lookupEnvIO,
  lookupEnv,
  lookupEnvUIO,
) where

-- import Data.ByteString (ByteString)
import Foreign.C.Types (CChar)
import Foreign.Ptr (Ptr, nullPtr)

import CheckedIO
import CheckedIO.Foreign (
  CString (..),
  EncodingError,
  fromCString,
  withCString,
 )

{----- getEnv -----}

data GetEnvVarError
  = GetEnvVarMissing String
  | GetEnvVarEncodingError EncodingError
  deriving (Show, Eq)

instance Exception GetEnvVarError where
  displayException = \case
    GetEnvVarMissing name -> "getEnv: environment variable does not exist: " ++ name
    GetEnvVarEncodingError e -> "getEnv: " ++ displayException e

getEnvIO :: MonadRunIO m => String -> m String
getEnvIO = uioToIO . getEnvUIO

getEnv :: MonadRunIOE GetEnvVarError m => String -> m String
getEnv = fromUIO . getEnvUIO

getEnvUIO :: MonadRunIOE e m => String -> m (Either GetEnvVarError String)
getEnvUIO name = go <$> lookupEnvUIO name
  where
    go = \case
      Right (Just res) -> Right res
      Right Nothing -> Left (GetEnvVarMissing name)
      Left e -> Left (GetEnvVarEncodingError e)

{----- lookupEnv -----}

-- | Same as 'lookupEnv', except in 'IO'.
lookupEnvIO :: MonadRunIO m => String -> m (Maybe String)
lookupEnvIO = uioToIO . lookupEnvUIO

-- | Same as 'lookupEnvUIO', except throwing the 'EncodingError'.
lookupEnv :: MonadRunIOE EncodingError m => String -> m (Maybe String)
lookupEnv = fromUIO . lookupEnvUIO

-- | Look up the given environment variable.
lookupEnvUIO :: MonadRunIOE e m => String -> m (Either EncodingError (Maybe String))
lookupEnvUIO name =
  fmap (either Left id) . runIOE $
    withCString name $ \(CString name') -> do
      res <- runUIO $ c_getenv name'
      if res == nullPtr
        then pure $ Right Nothing
        else fmap Just <$> fromCString (CString res)

-- TODO: add Windows support
foreign import ccall unsafe "getenv"
   c_getenv :: Ptr CChar -> UIO (Ptr CChar)
