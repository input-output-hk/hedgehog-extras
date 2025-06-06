{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Extras.Test.Prim
  ( failWithCustom
  , failMessage

  ) where

import           Data.Either (Either (..))
import           Data.Function
import           Data.Maybe
import           Data.Monoid (Monoid (..))
import           Data.String (String)
import           GHC.Stack
import           Hedgehog (MonadTest)
import           Hedgehog.Internal.Property (Diff, liftTest, mkTest)
import           Hedgehog.Internal.Source (getCaller)

import qualified Hedgehog.Internal.Property as H

{- HLINT ignore "Reduce duplication" -}

-- | Takes a 'CallStack' so the error can be rendered at the appropriate call site.
failWithCustom :: MonadTest m => CallStack -> Maybe Diff -> String -> m a
failWithCustom cs mdiff msg = liftTest $ mkTest (Left $ H.Failure (getCaller cs) msg mdiff, mempty)

-- | Takes a 'CallStack' so the error can be rendered at the appropriate call site.
failMessage :: MonadTest m => CallStack -> String -> m a
failMessage cs = failWithCustom cs Nothing
