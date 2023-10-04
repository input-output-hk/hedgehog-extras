module Hedgehog.Extras.Stock.Aeson
  ( rewriteObject
  , rewriteArrayElements
  ) where

import           Data.Aeson
import           Data.Functor
import           Data.HashMap.Lazy
import           Data.Text
import           Prelude (($), (.))

import qualified Data.Aeson.KeyMap as KM

-- | Rewrite a JSON object to another JSON object using the function 'f'.
--
-- All other JSON values are preserved.
rewriteObject :: (HashMap Text Value -> HashMap Text Value) -> Value -> Value
rewriteObject f (Object hm) = Object (KM.fromHashMapText . f . KM.toHashMapText $ hm)
rewriteObject _ v = v

-- | Rewrite each element of a JSON array using the function 'f'.
--
-- All other JSON values are preserved.
rewriteArrayElements :: (Value -> Value) -> Value -> Value
rewriteArrayElements f (Array hm) = Array (fmap f hm)
rewriteArrayElements _ v = v
