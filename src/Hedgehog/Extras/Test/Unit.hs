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
  ) where

import Control.Monad.Base
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Resource
import Data.Generics.Product.Any
import Data.Maybe
import Data.Monoid
import HaskellWorks.Prelude
import Hedgehog
import Hedgehog.Internal.Property qualified as H
import Hedgehog.Extras.Internal.Orphans ()
import Lens.Micro
import Test.Tasty.Discover
import Test.Tasty.Hedgehog (testProperty)
import Control.Monad.Morph

newtype UnitIO a = UnitIO { runTestIO :: TestT (ResourceT IO) a }
  deriving newtype (Applicative)
  deriving newtype (Functor)
  deriving newtype (Monad)
  deriving newtype (MonadBase IO)
  deriving newtype (MonadBaseControl IO)
  deriving newtype (MonadCatch)
  deriving newtype (MonadFail)
  deriving newtype (MonadIO)
  deriving newtype (MonadResource)
  deriving newtype (MonadTest)
  deriving newtype (MonadThrow)

instance Tasty (UnitIO ()) where
  tasty info = pure . testProperty testName . H.withTests 1 . H.property . hoist runResourceT . H.test .  runTestIO
    where testName = fromMaybe "" $ getLast (info ^. the @"name")
