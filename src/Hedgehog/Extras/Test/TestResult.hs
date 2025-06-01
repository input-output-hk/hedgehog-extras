module Hedgehog.Extras.Test.TestResult
  ( TestResult(..),
  ) where

import Hedgehog.Internal.Property qualified as H
import Control.Exception (SomeException)

data TestResult a
  = TestResult a
  | TestFailure H.Failure
  | TestError SomeException
