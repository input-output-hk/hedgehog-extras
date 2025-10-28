{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Hedgehog.Extras.Test.Unit
  ( UnitIO(..)
  , testUnitIO
  ) where

import Control.Monad.Base
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Resource
import Data.Maybe
import Data.Monoid
import Hedgehog
import Hedgehog.Extras.Internal.Orphans ()
import Hedgehog.Extras.Test.MonadAssertion (MonadAssertion)
import Hedgehog.Internal.Property qualified as H
import Prelude
import Test.Tasty.Discover
import Test.Tasty.Discover.TastyInfo qualified as TastyInfo
import Test.Tasty.Hedgehog (testProperty)

import qualified Test.Tasty as T

newtype UnitIO a = UnitIO { runTestIO :: TestT (ResourceT IO) a }
  deriving newtype (Applicative)
  deriving newtype (Functor)
  deriving newtype (Monad)
  deriving newtype (MonadAssertion)
  deriving newtype (MonadBase IO)
  deriving newtype (MonadBaseControl IO)
  deriving newtype (MonadCatch)
  deriving newtype (MonadFail)
  deriving newtype (MonadIO)
  deriving newtype (MonadResource)
  deriving newtype (MonadTest)
  deriving newtype (MonadThrow)

instance Tasty (UnitIO ()) where
  tasty info = pure . testUnitIO testName
    where testName = fromMaybe "" . getLast $ TastyInfo.name info

testUnitIO :: T.TestName -> UnitIO () -> T.TestTree
testUnitIO testName =
  testProperty testName . H.withTests 1 . H.property . hoist runResourceT . H.test . runTestIO
