{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- | This modules provides a tripwire abstraction. You can use tripwire as a detection mechanism if the code
-- path was executed. Trip a tripwire with 'trip' in the place where you'd like to detect if it was
-- reached. The tripwire can then be checked in the other place in the code using for example 'isTripped' or
-- 'assertNotTripped'.
module Hedgehog.Extras.Test.Tripwire
  (
  -- * Create a tripwire
    Tripwire
  , makeTripwire
  , makeTripwireWithLabel
  -- * Tripwire operations
  , trip
  , trip_
  , isTripped
  , getTripSite
  , resetTripwire
  -- * Assertions
  , assertNotTripped
  , assertTripped
  ) where

import           Control.Monad.IO.Class
import           GHC.Stack

import           Control.Concurrent.MVar
import           Control.Monad
import           Data.IORef
import           Data.Maybe
import           Hedgehog (MonadTest)
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Internal.Property as H
import           Prelude
import           System.IO.Unsafe (unsafePerformIO)

-- | Counter used to allocate consecutive IDs to tripwires
tripwireCounter :: IORef Int
tripwireCounter = unsafePerformIO $ newIORef 0
{-# NOINLINE tripwireCounter #-}

-- | Represents a tripwire which can be tripped only once. It can be used to detect if a particular code path
-- was reached.
data Tripwire = Tripwire
  { tripwireId :: !String -- ^ a label for identifying the tripwire
  , tripSite :: MVar CallStack -- ^ call stack of the trip site
  }

instance Show Tripwire where
  show Tripwire{tripwireId} = "Tripwire " <> tripwireId

-- | Creates a new tripwire
makeTripwire :: MonadIO m => m Tripwire
makeTripwire = liftIO $ do
  id' <- atomicModifyIORef' tripwireCounter (join (,) . (+1))
  Tripwire (show id') <$> newEmptyMVar

-- | Creates a new tripwire with a label, which is visible when 'show'ed: @Tripwire mylabel@
makeTripwireWithLabel :: MonadIO m
                      => String
                      -> m Tripwire
makeTripwireWithLabel label = liftIO $ do
  Tripwire label <$> newEmptyMVar

-- | Triggers the tripwire and registers the place of the first trigger. Idempotent.
-- Prints the information in the test log about tripping the tripwire.
trip :: HasCallStack
     => MonadIO m
     => MonadTest m
     => Tripwire
     -> m ()
trip t@Tripwire{tripSite} = withFrozenCallStack $ do
  H.note_ $ show t <> " has been tripped"
  void . liftIO $ tryPutMVar tripSite callStack

-- | Triggers the tripwire and registers the place of the first trigger. Idempotent. A silent variant of
-- 'trip' which does not require 'MonadTest', but also does not log the information about tripping.
trip_ :: HasCallStack
      => MonadIO m
      => Tripwire
      -> m ()
trip_ Tripwire{tripSite} = withFrozenCallStack $ do
  void . liftIO $ tryPutMVar tripSite callStack

-- | Restore tripwire to initial non triggered state
resetTripwire :: MonadIO m
              => Tripwire
              -> m ()
resetTripwire Tripwire{tripSite} = liftIO $ void $ tryTakeMVar tripSite

-- | Return the call stack, where the tripwire was tripped - if it was tripped.
getTripSite :: MonadIO m
            => Tripwire
            -> m (Maybe CallStack)
getTripSite Tripwire{tripSite} = liftIO $ tryReadMVar tripSite

-- | Check if the tripwire was tripped.
isTripped :: MonadIO m
          => Tripwire
          -> m Bool
isTripped Tripwire{tripSite} = liftIO $ not <$> isEmptyMVar tripSite

-- | Fails the test if the tripwire was triggered. Prints the call stack where the tripwire was triggered.
assertNotTripped :: HasCallStack
                 => MonadTest m
                 => MonadIO m
                 => Tripwire
                 -> m ()
assertNotTripped tripwire = withFrozenCallStack $ do
  mTripSite <- getTripSite tripwire
  forM_ mTripSite $ \cs -> do
    H.note_ $ show tripwire <> " has been tripped at: " <> prettyCallStack cs
    H.failure

-- | Fails the test if the tripwire was not triggered yet.
assertTripped :: HasCallStack
              => MonadTest m
              => MonadIO m
              => Tripwire
              -> m ()
assertTripped tripwire = withFrozenCallStack $ do
  mTripSite <- getTripSite tripwire
  when (isNothing mTripSite) $ do
    H.note_ $ show tripwire <> " was not tripped"
    H.failure


