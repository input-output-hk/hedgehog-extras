{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
module Hedgehog.Extras.Test.TestWatchdogSpec where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Control.Monad.Trans.Writer.Lazy (runWriterT)
import           Data.Function
import           Data.List (isPrefixOf)
import           Data.Time.Clock as D
import           GHC.Conc
import           GHC.Stack
import           Hedgehog (Property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock as H
import           Hedgehog.Extras.Test.TestWatchdog
import qualified Hedgehog.Internal.Property as H
import           Prelude
import qualified System.Process as P

-- | Check that watchdog kills test case which waits without an end
hprop_check_watchdog_kills_hanged_thread :: Property
hprop_check_watchdog_kills_hanged_thread = H.propertyOnce $ do
  let watchdogCfg = WatchdogConfig 1
  childTid <- H.newEmptyMVar
  tripwire <- H.makeTripwire

  (result, _) <- spawnTestT $ runWithWatchdog_ watchdogCfg $ do
    liftIO $ myThreadId >>= H.putMVar childTid
    -- simulate thread hang
    void $ H.threadDelay 3_000_000
    H.trip tripwire

  assertWatchdogExceptionWasRaised result

  -- make sure that we  didn't trigger the tripwire
  H.assertNotTripped tripwire
  childStatus <- liftIO $ H.readMVar childTid >>= threadStatus
  childStatus === ThreadFinished

-- | Check that watchdog kills test case which spawns:
--  - a process waiting forever
--  - a child threads waiting
hprop_check_watchdog_kills_hanged_thread_with_its_children :: Property
hprop_check_watchdog_kills_hanged_thread_with_its_children = H.propertyOnce $ do
  let watchdogCfg = WatchdogConfig 1
  childTid <- H.newEmptyMVar
  grandChildTid1 <- H.newEmptyMVar
  grandChildTid2 <- H.newEmptyMVar
  procHandle <- H.newEmptyMVar
  childTripwire <- H.makeTripwire
  grandChildTripwire1 <- H.makeTripwire
  grandChildTripwire2 <- H.makeTripwire

  (result, _) <- spawnTestT $ runWithWatchdog_ watchdogCfg $ do
    liftIO $ myThreadId >>= H.putMVar childTid

    unless H.isWin32 $ do
      (_, _, _, h, _) <- H.createProcess $ P.shell "tail -f /dev/null"
      H.putMVar procHandle h

    H.asyncRegister_ $ do
      liftIO $ myThreadId >>= H.putMVar grandChildTid1
      threadDelay 3_000_000
      H.trip_ grandChildTripwire1

    H.asyncRegister_ $ do
      liftIO $ myThreadId >>= H.putMVar grandChildTid2
      threadDelay 3_000_000
      H.trip_ grandChildTripwire2

    void $ H.threadDelay 3_000_000
    H.trip childTripwire

  -- make sure that we didn't trigger the tripwire
  H.assertNotTripped childTripwire
  H.assertNotTripped grandChildTripwire1
  H.assertNotTripped grandChildTripwire2

  assertWatchdogExceptionWasRaised result

  -- Give OS 5 seconds to do the process cleanup
  deadline <- D.addUTCTime 5 <$> liftIO D.getCurrentTime

  H.byDeadlineM 0.2 deadline "childStatus" $ do
    childStatus <- liftIO $ H.readMVar childTid >>= threadStatus
    childStatus === ThreadFinished

  H.byDeadlineM 0.2 deadline "grandChildStatus1" $ do
    grandChildStatus1 <- liftIO $ H.readMVar grandChildTid1 >>= threadStatus
    grandChildStatus1 === ThreadFinished

  H.byDeadlineM 0.2 deadline "grandChildStatus2" $ do
    grandChildStatus2 <- liftIO $ H.readMVar grandChildTid2 >>= threadStatus
    grandChildStatus2 === ThreadFinished

  -- check that tail process got killed
  unless H.isWin32 $
    H.byDeadlineM 0.2 deadline "tailPid" $ do
      tailPid <- liftIO $ H.readMVar procHandle >>= P.getPid
      tailPid === Nothing

assertWatchdogExceptionWasRaised :: HasCallStack
                                 => H.MonadTest m
                                 => MonadFail m
                                 => Either H.Failure a
                                 -> m ()
assertWatchdogExceptionWasRaised = withFrozenCallStack $ \case
  Right _ -> do
    H.note_ "Expected failure instead of Right"
    H.failure
  Left (H.Failure _ msg _) -> do
    -- check we've failed because of watchdog
    _header:exception:_ <- pure $ lines msg
    H.note_ $ "Received exception:"
    H.assertWith exception $
      isPrefixOf "WatchdogException: "


-- | Spawn TestT in an async. Waits for the async and logs the result as well as errors journal on failure
spawnTestT :: HasCallStack
         => H.MonadTest m
         => MonadIO m
         => Show a
         => H.TestT (ResourceT IO) a
         -> m (Either H.Failure a, H.Journal)
spawnTestT testt = withFrozenCallStack $ do
  (res, log') <- H.evalIO $
    H.withAsync
      (runResourceT . runWriterT . runExceptT $ H.unTest testt)
      H.wait
  H.noteShow_ res
  H.noteShow_ log'
  pure (res, log')

