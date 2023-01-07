{-# LANGUAGE DeriveGeneric #-}

module Hedgehog.Extras.Internal.Test.Integration
  ( Integration
  , IntegrationState(..)
  , newIntegrationStateIO
  , newIntegrationStateM
  , runIntegrationReaderT
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader (ReaderT(runReaderT) )
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Functor ( (<$>) )
import           GHC.Generics (Generic)
import           System.IO (IO)

import qualified Control.Concurrent.STM as STM
import qualified Hedgehog as H

newtype IntegrationState = IntegrationState
  { integrationStateFinals :: STM.TVar [Integration ()]
  } deriving (Generic)

type Integration a = H.PropertyT (ReaderT IntegrationState (ResourceT IO)) a

newIntegrationStateIO :: IO IntegrationState
newIntegrationStateIO = IntegrationState <$> STM.newTVarIO []

newIntegrationStateM :: MonadIO m => m IntegrationState
newIntegrationStateM = liftIO newIntegrationStateIO

runIntegrationReaderT :: MonadIO m => ReaderT IntegrationState m a -> m a
runIntegrationReaderT f = do
  s <- newIntegrationStateM
  runReaderT f s
