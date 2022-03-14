module Hedgehog.Extras.Stock.Aeson
  ( rewriteObject
  ) where

import           Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import           Data.HashMap.Lazy
import           Data.Text
import           Prelude ((.), ($))

-- | Rewrite a JSON object to another JSON object using the function 'f'.
--
-- All other JSON values are preserved.
rewriteObject :: (HashMap Text Value -> HashMap Text Value) -> Value -> Value
rewriteObject f (Object hm) = Object (KM.fromHashMapText . f . KM.toHashMapText $ hm)
rewriteObject _ v = v
