{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Hedgehog.Extras.Internal.Orphans () where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import System.IO (IO)
import UnliftIO qualified

instance MonadBase IO (ResourceT IO) where
  liftBase = liftIO

instance MonadBaseControl IO (ResourceT IO) where
  type StM (ResourceT IO) a = a
  liftBaseWith = UnliftIO.withRunInIO
  restoreM = pure

