{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}


{- | This modules provides concurrency abstractions for hedgehog tests. Using "lifted-base" one can execute
expensive test actions concurrently.

For example, the actions invoked inside 'mapConcurrently_' are invoked in the same 'MonadTest' as the outer
monad of 'mapConcurrently_'.

@
import qualified Hedgehog.Extras.Test.Concurrent as H

setUpEnvironment = H.mapConcurrently_ id
  [ H.threadDelay 100 >> pure 1
  , H.threadDelay 200 >> pure 2
  , H.threadDelay 300 >> pure 3
  ]
@


__Warning: Do not use this module for running concurrent checks!__ The 'MonadBaseControl' instance does not
aggregate effects for 'PropertyT'. Consider the following code:

@
  LA.mapConcurrently_ id
    [ do
      H.note_ \"FAIL1\"
      success
    , do
      IO.threadDelay 1_000_000
      H.note_ \"FAIL2\"
      failure
    , do
      H.note_ \"FAIL3\"
      failure
    ]
@

Executing this code will give you the following output in the test report:

@
66 ┃   LA.mapConcurrently_ id
67 ┃     [ do
68 ┃       H.note_ \"FAIL1\"
   ┃       │ FAIL1
69 ┃       success
70 ┃     , do
71 ┃       IO.threadDelay 1_000_000
72 ┃       H.note_ \"FAIL2\"
   ┃       │ FAIL2
73 ┃       failure
   ┃       ^^^^^^^
74 ┃     , do
75 ┃       H.note_ \"FAIL3\"
76 ┃       failure
77 ┃     ]
@
Please note that only @FAIL1@ and @FAIL2@ annotations were reported - @FAIL3@ annotation and the failure
below was swallowed without any information.

__Don't use concurrency abstractions from this module, when you need to aggregate and report failures!__

-}
module Hedgehog.Extras.Test.Concurrent
  ( threadDelay
  , asyncRegister_
  -- * Re-exports of concurrency abstractions from @lifted-base@
  , module Control.Concurrent.Async.Lifted
  , module Control.Concurrent.MVar.Lifted
  , module System.Timeout.Lifted
  ) where

import           Control.Applicative
import           Control.Concurrent.Async.Lifted
import qualified Control.Concurrent.Lifted as IO
import           Control.Concurrent.MVar.Lifted
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Data.Function
import           Data.Int
import qualified GHC.Stack as GHC
import           System.IO (IO)
import           System.Timeout.Lifted
import qualified UnliftIO

import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           GHC.Stack
import           Hedgehog
import qualified Hedgehog as H

-- | Delay the thread by 'n' microseconds.
threadDelay :: (HasCallStack, MonadTest m, MonadIO m) => Int -> m ()
threadDelay n = GHC.withFrozenCallStack . H.evalIO $ IO.threadDelay n

-- | Runs an action in background, and registers its cancellation to 'MonadResource'.
asyncRegister_ :: HasCallStack
               => MonadTest m
               => MonadResource m
               => MonadCatch m
               => IO a -- ^ Action to run in background
               -> m ()
asyncRegister_ act = GHC.withFrozenCallStack $ void . H.evalM $ allocate (async act) cleanUp
  where
    cleanUp :: Async a -> IO ()
    cleanUp a = cancel a >> void (link a)

instance MonadBase IO (ResourceT IO) where
  liftBase = liftIO

instance MonadBaseControl IO (ResourceT IO) where
  type StM (ResourceT IO) a = a
  liftBaseWith = UnliftIO.withRunInIO
  restoreM = pure
