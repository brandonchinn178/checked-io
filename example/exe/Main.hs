{-# LANGUAGE LambdaCase #-}

import Example

main :: Main
main = uncheckIOE $ do
  putStrLn "Starting program"

  putStrLn "Running foo..."
  foo

  putStrLn "Type in a number:"
  input <- getLine
  try (decodeInt input) >>= \case
    Left _ -> putStrLn "You did not type in a number!"
    Right x -> putStrLn $ "You typed in: " ++ show x
