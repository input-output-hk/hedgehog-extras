module Hedgehog.Extras.Test.TestExpectFailure where

import           Prelude
import           Hedgehog (property, Property, MonadTest, (===), success)
import           Hedgehog.Extras.Test.Base (expectFailureWith, expectFailure)
import           Hedgehog.Internal.Property (Failure (..), failWith)
import           GHC.Stack (HasCallStack)
import           Control.Monad.IO.Class (MonadIO)

hprop_expect_always_fails_prop :: Property
hprop_expect_always_fails_prop = property $ do
  expectFailureWith failureCheck alwaysFailsProp
  where
    failureCheck :: (MonadTest m, HasCallStack) => Failure -> m ()
    failureCheck (Failure _ reason _) =
      reason === "This property always fails"

    alwaysFailsProp :: (MonadTest m, HasCallStack) => m ()
    alwaysFailsProp = failWith Nothing "This property always fails"

hprop_bad_expect_always_fails_fails_prop :: Property
hprop_bad_expect_always_fails_fails_prop = property $ do
  expectFailure badExpectFailure
  where
    badExpectFailure :: (MonadIO m, MonadTest m, HasCallStack) => m ()
    badExpectFailure = expectFailureWith badFailureCheck alwaysFailsProp

    badFailureCheck :: (MonadTest m, HasCallStack) => Failure -> m ()
    badFailureCheck (Failure _ reason _) =
      reason === "This property sometimes fails"

    alwaysFailsProp :: (MonadTest m, HasCallStack) => m ()
    alwaysFailsProp = failWith Nothing "This property always fails"

hprop_bad_expect_failure_fails_prop :: Property
hprop_bad_expect_failure_fails_prop = property $ do
  expectFailure (expectFailure success)
