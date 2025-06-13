{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hedgehog.Extras.Internal.Plan
  ( Plan(..)
  , Component(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as M
import           Data.Eq
import           Data.Function
import           Data.Maybe
import           Data.Text (Text)
import           GHC.Generics
import           Text.Show

data Component = Component
  { componentName :: Maybe Text
  , binFile :: Maybe Text
  , components :: [Component]
  }
  deriving (Generic, Eq, Show)

newtype Plan = Plan
  { installPlan :: [Component]
  }
  deriving (Generic, Eq, Show)

instance FromJSON Plan where
    parseJSON = withObject "Plan" $ \v -> Plan
        <$> v .: "install-plan"

instance FromJSON Component where
    parseJSON = withObject "Plan" $ \v -> do
      componentName <- v .:? "component-name"
      binFile <- v .:? "bin-file"
      componentsTuples <- join . maybeToList . fmap M.toAscList <$> v .:? "components"
      -- sub-components are an object with components name as a key
      components <- forM componentsTuples $ \(subComponentName, subComponent) ->
        parseJSON $
          A.Object $
            M.insert "component-name" (toJSON subComponentName) subComponent
      pure Component{..}

