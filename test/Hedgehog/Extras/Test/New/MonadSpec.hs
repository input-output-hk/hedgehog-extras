module Hedgehog.Extras.Test.New.MonadSpec where

import Control.Monad.Catch
import HaskellWorks.Prelude
import Hedgehog hiding (forAll)
import Hedgehog.Extras.Test.New.UnitIO

tasty_unit :: UnitIO ()
tasty_unit = do
  bracket_
    (True === True)
    (True === True)
    (True === True)
