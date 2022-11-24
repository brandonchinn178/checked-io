{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module CheckedIO.Foreign (
  CString (..),
  withCString,
  fromCString,

  -- * Encodings
  EncodingError (..),
) where

import qualified Foreign.C.String as GHC
import Foreign.C.Types (CChar)
import Foreign.Ptr (Ptr)
import qualified GHC.IO.Exception as GHC

import CheckedIO

-- | A @Ptr CChar@ with a state @s@ to ensure that it's not used
-- after being destroyed.
newtype CString s = CString { unCString :: Ptr CChar }

withCString ::
  MonadRunAsIOE e m =>
  String
  -> (forall s. CString s -> m a)
  -> m (Either EncodingError a)
withCString s f =
  withRunAsIOE $ \run ->
    checkUIOWith checkE $ GHC.withCString s (uncheckIOE . run . f . CString)
  where
    checkE e =
      case fromException e of
        Just (GHC.IOError _ _ "recoverEncode" msg _ _) -> EncodingError msg
        _ -> error $ "Unexpected encoding error: " ++ show e

fromCString :: MonadRunIOE e m => CString s -> m (Either EncodingError String)
fromCString = checkUIOWith checkE . GHC.peekCString . unCString
  where
    checkE e =
      case fromException e of
        Just (GHC.IOError _ _ "recoverDecode" msg _ _) -> EncodingError msg
        _ -> error $ "Unexpected encoding error: " ++ show e

{----- Encodings -----}

data EncodingError = EncodingError String
  deriving (Show)

instance Exception EncodingError
