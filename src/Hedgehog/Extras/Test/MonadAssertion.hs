{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hedgehog.Extras.Test.MonadAssertion
  ( MonadAssertion(..)
  , assertFailure
  , assertFailure_
  , tryAssertion
  ) where

import           Control.Applicative (Applicative(..))
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Either
import           Data.Function
import           Data.Functor ((<$>))
import           Data.Monoid
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest(..))
import           Hedgehog.Extras.Test.Prim
import           Text.Show (Show(..))

import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Resource as IO
import qualified Control.Monad.Trans.Resource.Internal as IO
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H

class Monad m => MonadAssertion m where
  throwAssertion :: H.Failure -> m a
  catchAssertion :: m a -> (H.Failure -> m a) -> m a

instance Monad m => MonadAssertion (H.TestT m) where
  throwAssertion f = H.liftTest $ H.mkTest (Left f, mempty)
  catchAssertion g h = H.TestT $ E.catchE (H.unTest g) (H.unTest . h)

instance MonadAssertion m => MonadAssertion (IO.ResourceT m) where
  throwAssertion = lift . throwAssertion
  catchAssertion r h = IO.ResourceT $ \i -> IO.unResourceT r i `catchAssertion` \e -> IO.unResourceT (h e) i

deriving instance Monad m => MonadAssertion (H.PropertyT m)

-- | Attempt to run a function that may assert, returning either a failure or the result of the assertion.
tryAssertion :: ()
  => MonadAssertion m
  => m a
  -> m (Either H.Failure a)
tryAssertion m =
  catchAssertion (Right <$> m) (pure . Left)

-- | Run the given action and succeed if the action fails, but fail if it succeeds.
assertFailure :: ()
  => HasCallStack
  => Show a
  => MonadAssertion m
  => MonadTest m
  => m a
  -> m H.Failure
assertFailure f = do
  result <- tryAssertion f
  case result of
    Left e -> pure e
    Right a -> failMessage GHC.callStack $ show a

-- | Run the given action and succeed if the action fails, but fail if it succeeds.
assertFailure_ :: ()
  => HasCallStack
  => Show a
  => MonadAssertion m
  => MonadTest m
  => m a
  -> m ()
assertFailure_ f =
  GHC.withFrozenCallStack $
    void $ assertFailure f
