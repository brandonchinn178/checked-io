{- AUTOCOLLECT.MAIN
custom_main = true
strip_suffix = Test
-}

import Test.Tasty

{- AUTOCOLLECT.MAIN.imports -}

main :: Main
main = defaultMain $ testGroup "checked-io" {- AUTOCOLLECT.MAIN.tests -}
