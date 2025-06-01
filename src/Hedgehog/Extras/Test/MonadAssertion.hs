{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hedgehog.Extras.Test.MonadAssertion
  ( MonadAssertion(..),
    tryAssertion,
    tryExceptAssertion,
    getTestResult,
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch (MonadCatch (..), MonadThrow (..))
import           Control.Monad.Catch qualified as C
import           Control.Monad.Error.Class
import           Control.Monad.Trans.Class
import           Data.Either
import           Data.Function
import           Data.Monoid (mempty)
import           Hedgehog.Extras.Test.TestResult

import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Resource as IO
import qualified Control.Monad.Trans.Resource.Internal as IO
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Property as H

-- | A monad that supports assertions, allowing for throwing and catching assertion failures.
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

-- | Attempt to run a function that may throw an exception, returning a `TestResult` that encapsulates the
-- result, failure, or error.
tryExceptAssertion :: ()
  => MonadAssertion m
  => MonadCatch m
  => m a
  -> m (TestResult a)
tryExceptAssertion m =
  tryAssertion (C.try m) >>= \case
    Right (Right a) -> pure $ TestResult a
    Right (Left e) -> pure $ TestError e
    Left f -> pure $ TestFailure f

-- | Extract the result from a `TestResult`, throwing an error if the result is a failure or an exception.
getTestResult :: ()
  => MonadThrow m
  => MonadError H.Failure m
  => TestResult a
  -> m a
getTestResult = \case
  TestResult a -> pure a
  TestFailure f -> throwError f
  TestError e -> throwM e
