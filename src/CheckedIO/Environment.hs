-- | A checked version of @System.Environment@
module CheckedIO.Environment (
  -- * lookupEnv
  lookupEnvIO,
  lookupEnv,
  lookupEnvUIO,

  -- * Helpers
  EncodingError (..),
  decodeBytesWith,
) where

import Data.ByteString (ByteString)
import GHC.IO.Encoding (TextEncoding)

import CheckedIO

lookupEnvIO :: String -> IO (Maybe String)
lookupEnvIO = undefined

lookupEnv :: String -> IOE EncodingError (Maybe String)
lookupEnv = undefined

lookupEnvUIO :: String -> UIO (Maybe ByteString)
lookupEnvUIO = undefined

{----- Helpers (TODO: move to another module) -----}

data EncodingError = EncodingError

decodeBytesWith :: TextEncoding -> ByteString -> UIO (Either EncodingError String)
decodeBytesWith = undefined
