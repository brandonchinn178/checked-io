-- TODO: autogenerate imports + test tree
-- https://github.com/brandonchinn178/tasty-autocollect/issues/38

import Test.Tasty

import qualified CheckedIO.CoreTest
import qualified CheckedIO.EnvironmentTest

main :: Main
main = defaultMain test

test :: TestTree
test =
  testGroup "checked-io" $
    [ testGroup "CheckedIO.Core" CheckedIO.CoreTest.tasty_autocollect_tests
    , testGroup "CheckedIO.Environment" CheckedIO.EnvironmentTest.tasty_autocollect_tests
    ]
