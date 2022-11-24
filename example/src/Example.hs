module Example where

import CheckedIO.Environment (lookupEnvIO)
import Text.Read (readMaybe)

foo :: IO ()
foo = do
  putStrLn "hello world!"
  user <- lookupEnvIO "USER"
  putStrLn $
    case user of
      Nothing -> "$USER is not set"
      Just user' -> "$USER is set to: " ++ user'

data DecodeError = DecodeError String
  deriving (Show)

instance Exception DecodeError

decodeInt :: String -> IOE DecodeError Int
decodeInt s =
  case readMaybe s of
    Just x -> pure x
    Nothing -> throw $ DecodeError $ "Not an int: " ++ s
