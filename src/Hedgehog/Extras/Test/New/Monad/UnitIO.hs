{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module Hedgehog.Extras.Test.New.Monad.UnitIO
  ( UnitIO(..)
  , UnitEnv(..)
  , unitIOToTestT
  ) where

import Control.Concurrent qualified as IO
import Control.Concurrent.STM
import Control.Concurrent.STM qualified as STM
import Control.Exception as IO
import Control.Exception.Lifted qualified as CEL
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource (ResourceT, MonadResource(..), runResourceT)
import Data.Either
import Data.Generics.Product.Any
import Data.Maybe
import Data.Monoid
import HaskellWorks.Control.Monad
import HaskellWorks.Prelude
import Hedgehog
import Hedgehog.Extras.Test.MonadAssertion
import Hedgehog.Internal.Property qualified as H
import Lens.Micro
import Test.Tasty.Discover
import Test.Tasty.Hedgehog (testProperty)

newtype UnitEnv = UnitEnv { unUnitEnv :: TMVar (TestT IO ()) }

newtype UnitIO a = UnitIO { runTestIO :: ExceptT H.Failure (ResourceT (ReaderT UnitEnv IO)) a }
  deriving newtype (Applicative)
  deriving newtype (Functor)
  deriving newtype (Monad)
  deriving newtype (MonadCatch)
  deriving newtype (MonadError H.Failure)
  deriving newtype (MonadFail)
  deriving newtype (MonadFix)
  deriving newtype (MonadIO)
  deriving newtype (MonadMask)
  deriving newtype (MonadReader UnitEnv)
  deriving newtype (MonadResource)
  deriving newtype (MonadThrow)

instance MonadTest UnitIO where
  liftTest f = do
    UnitEnv tmvAction <- ask @UnitEnv
    -- Create a new TMVar in 'tmvA' to hold the result of the test action.
    tmvA <- liftIO STM.newEmptyTMVarIO
    liftIO $ STM.atomically $ putTMVar tmvAction $
      -- Send the action action to run by the 'PropertyT' thread.  This action will run the test
      -- and put the result in the TMVar 'tmvA'.  As the action is run in a separate thread, its
      -- result cannot be simply returned.
      tryExceptAssertion (liftTest f) >>= liftIO . STM.atomically . STM.putTMVar tmvA
    -- Extract the result of the test action from the TMVar 'tmvA'.  This will block until the
    -- action has completed and the result has been put in the TMVar.
    testResult <- liftIO $ STM.atomically $ STM.takeTMVar tmvA
    getTestResult testResult

instance Tasty (UnitIO ()) where
  tasty info = pure . testProperty testName . H.withTests 1 . H.property . H.test . unitIOToTestT
    where testName = fromMaybe "" $ getLast (info ^. the @"name")

unitIOToTestT :: ()
  => UnitIO ()
  -> H.TestT IO ()
unitIOToTestT f = do
  tvResult <- liftIO STM.newEmptyTMVarIO
  tvAction <- liftIO STM.newEmptyTMVarIO
  -- Fork a thread to run the test code.  The test code cannot run in the existing thread because that
  -- thread is running in 'PropertyT' which does not implement 'MonadMask'.
  CEL.bracket
    do  liftIO $ IO.forkFinally
          do  f & runTestIO
                & runExceptT
                & runResourceT
                & flip runReaderT (UnitEnv tvAction)
          (liftIO . STM.atomically . STM.putTMVar tvResult)
    do liftIO . IO.killThread
    do \_ -> do
        whileNothingM do
          actionOrResult <- liftIO $ STM.atomically do
            mAction <- STM.tryTakeTMVar tvAction
            case mAction of
              Nothing -> do
                mResult <- STM.tryTakeTMVar tvResult
                case mResult of
                  Just a -> pure $ Right a
                  Nothing -> retry
              Just action -> pure $ Left action

          case actionOrResult of
            Left action -> action >> pure Nothing
            Right (Left e) -> liftIO $ IO.throwIO e
            Right (Right (Left e)) -> throwAssertion e
            Right (Right (Right a)) -> pure $ Just a
