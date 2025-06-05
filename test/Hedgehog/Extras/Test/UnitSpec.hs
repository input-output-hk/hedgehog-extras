module Hedgehog.Extras.Test.UnitSpec where

import Control.Exception.Lifted
import Data.Bool
import Hedgehog
import Hedgehog.Extras.Test.Unit

tasty_unit :: UnitIO ()
tasty_unit = do
  bracket_
    (True === True)
    (True === True)
    (True === True)
