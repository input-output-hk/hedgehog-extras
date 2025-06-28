{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides a test watchdog - an utility monitoring test cases and killing them if they don't
-- finish in time. 'Watchdog' thread runs in the background, and after specified timeout, it throws
-- 'WatchdogException' to the target thread. A user is able to 'kickWatchdog', which delays the killing and
-- 'poisonWatchdog' which stops the watchdog.
--
-- To wrap a test case in a watchdog just use
--
-- @
-- runWithWatchdog watchdogConfig $ \\watchdog -> do
--   -- body of your test case
-- @
--
module Hedgehog.Extras.Test.TestWatchdog
  (
  -- * Wrap in watchdog
    runWithWatchdog_
  , runWithWatchdog
  , runWithDefaultWatchdog_
  , runWithDefaultWatchdog

  -- * Watchdog control
  , kickWatchdog
  , poisonWatchdog

  -- * Types
  , Watchdog
  , WatchdogConfig(..)
  , WatchdogException(..)

  -- * Low level API
  -- | There is also a lower-level API available, giving the ability to provide target thread ID, which watchdog
  -- will try to kill.

  , makeWatchdog
  , runWatchdog
  ) where

import           Control.Concurrent (myThreadId, threadDelay, throwTo)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TChan (TChan, newTChanIO, tryReadTChan, writeTChan)
import           Control.Exception (Exception)
import           Control.Monad.IO.Class
import           Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime,
                   nominalDiffTimeToSeconds)
import           GHC.Conc (ThreadId)
import           GHC.Stack

import           Control.Monad.Base (MonadBase (..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Hedgehog.Extras.Test.Concurrent as H
import           Prelude

-- | Configuration for the watchdog.
newtype WatchdogConfig = WatchdogConfig
  { watchdogTimeout :: Int -- ^ Timeout in seconds after which watchdog will kill the test case
  }

-- | Default watchdog configuration with 10 minutes timeout.
defaultWatchdogConfig :: WatchdogConfig
defaultWatchdogConfig = WatchdogConfig
  { watchdogTimeout = 600
  }

-- | A watchdog instance. See the module header for more detailed description.
data Watchdog = Watchdog
  { watchdogConfig :: !WatchdogConfig
  , watchedThreadId :: !ThreadId -- ^ monitored thread id
  , startTime :: !UTCTime -- ^ watchdog creation time
  , kickChan :: TChan WatchdogCommand -- ^ a queue of watchdog commands
  }

instance Show Watchdog where
  show Watchdog{watchdogConfig=WatchdogConfig{watchdogTimeout}, startTime, watchedThreadId} = mconcat
    [ "Watchdog with timeout ", show watchdogTimeout
    , ", started at ", show startTime
    , ", watching thread ID ", show watchedThreadId
    ]

-- | Create manually a new watchdog, providing the target thread ID. After all watchdog timeouts expire,
-- the target thread will get 'WatchdogException' thrown to it asynchronously (using 'throwTo').
makeWatchdog :: MonadBase IO m
             => WatchdogConfig
             -> ThreadId -- ^ thread id which will get killed after all kicks expire
             -> m Watchdog
makeWatchdog config watchedThreadId' = liftBase $ do
  watchdog <- Watchdog config watchedThreadId' <$> getCurrentTime <*> newTChanIO
  kickWatchdog watchdog
  pure watchdog

getCallerLocation :: HasCallStack => String
getCallerLocation =
  case getCallStack callStack of
    (_funcName, _) : (_callerName, callerLoc) : _ ->
      srcLocFile callerLoc ++ ":" ++ show (srcLocStartLine callerLoc)
    _ -> "<no call stack>"

-- | Run watchdog in a loop in the current thread. Usually this function should be used with 'H.withAsync'
-- to run it in the background.
runWatchdog
  :: HasCallStack
  => MonadBase IO m
  => Watchdog
  -> m ()
runWatchdog w@Watchdog{watchedThreadId, startTime, kickChan} =
  withFrozenCallStack $ liftBase $ do
    atomically (tryReadTChan kickChan) >>= \case
      Just PoisonPill ->
        -- deactivate watchdog
        pure ()
      Just (Kick timeout) -> do
        -- got a kick, wait for another period
        threadDelay $ timeout * 1_000_000
        runWatchdog w
      Nothing -> do
        -- we are out of scheduled timeouts, kill the monitored thread
        currentTime <- getCurrentTime
        liftIO $ appendFile "/tmp/watchdog.log" $ mconcat
          [ "Watchdog: killing thread " <> show watchedThreadId
          , " after " <> show (diffUTCTime currentTime startTime)
          , "at " <> getCallerLocation
          , "\n"
          ]
        throwTo watchedThreadId . WatchdogException $ diffUTCTime currentTime startTime

-- | Watchdog command
data WatchdogCommand
  = Kick !Int -- ^ Add another delay in seconds
  | PoisonPill -- ^ Stop the watchdog

-- | Enqueue a kick for the watchdog. It will extend the timeout by another one defined in the watchdog
-- configuration.
kickWatchdog :: MonadIO m => Watchdog -> m ()
kickWatchdog Watchdog{watchdogConfig=WatchdogConfig{watchdogTimeout}, kickChan} = liftIO $
  atomically $ writeTChan kickChan (Kick watchdogTimeout)

-- | Enqueue a poison pill for the watchdog. It will stop the watchdog after all timeouts.
poisonWatchdog :: MonadIO m => Watchdog -> m ()
poisonWatchdog Watchdog{kickChan} = liftIO $
  atomically $ writeTChan kickChan PoisonPill


-- | Execute a test case with a watchdog.
runWithWatchdog :: HasCallStack
                => MonadBaseControl IO m
                => WatchdogConfig -- ^ configuration
                -> (HasCallStack => Watchdog -> m a) -- ^ a test case to be wrapped in watchdog
                -> m a
runWithWatchdog config testCase = do
  watchedThreadId <- liftBase myThreadId
  watchdog <- liftBase $ makeWatchdog config watchedThreadId
  H.withAsync (runWatchdog watchdog) $
    \_ -> testCase watchdog

-- | Execute a test case with a watchdog.
runWithWatchdog_ :: HasCallStack
                 => MonadBaseControl IO m
                 => WatchdogConfig -- ^ configuration
                 -> (HasCallStack => m a) -- ^ a test case to be wrapped in watchdog
                 -> m a
runWithWatchdog_ config testCase = runWithWatchdog config (const testCase)

-- | Execute a test case with watchdog with default config.
runWithDefaultWatchdog :: HasCallStack
                       => MonadBaseControl IO m
                       => (HasCallStack => Watchdog -> m a) -- ^ a test case to be wrapped in watchdog
                       -> m a
runWithDefaultWatchdog = runWithWatchdog defaultWatchdogConfig

-- | Execute a test case with watchdog with default config.
runWithDefaultWatchdog_ :: HasCallStack
                        => MonadBaseControl IO m
                        => (HasCallStack => m a) -- ^ a test case to be wrapped in watchdog
                        -> m a
runWithDefaultWatchdog_ testCase = runWithDefaultWatchdog (const testCase)

-- | An exception thrown to the test case thread.
newtype WatchdogException = WatchdogException { timeElapsed :: NominalDiffTime }

instance Show WatchdogException where
  show WatchdogException{timeElapsed} =
    "WatchdogException: Test watchdog killed test case thread after " <> show @Int (round $ nominalDiffTimeToSeconds timeElapsed) <> " seconds."

instance Exception WatchdogException
