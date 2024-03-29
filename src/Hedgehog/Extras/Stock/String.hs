module Hedgehog.Extras.Stock.String
  ( strip
  , lastLine
  , firstLine
  , readNoteM
  ) where

import           Control.Monad.Catch (MonadCatch)
import           Data.Bifunctor
import           Data.Function
import           Data.Semigroup
import           Data.String
import           GHC.Stack
import           Text.Read
import           Text.Show (Show)

import qualified Data.List as L
import qualified Data.Text as T
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H

-- | Strip whitespace from the beginning and end of the string.
strip :: String -> String
strip = T.unpack . T.strip . T.pack

-- | Get the last line in the string
lastLine :: String -> String
lastLine = strip . L.unlines . L.reverse . L.take 1 . L.reverse . L.lines

-- | Get the first line in the string
firstLine :: String -> String
firstLine = strip . L.unlines . L.take 1 . L.lines

-- | Trim leading and trailing whitespace and read the string into a value. Report the read value in the test
-- annotations.
readNoteM :: (Read a, Show a, H.MonadTest m, MonadCatch m, HasCallStack) => String -> m a
readNoteM inputStr =
  withFrozenCallStack
  $ H.noteShowM
  . H.evalEither
  . first (<> ": " <> inputStr)
  . readEither
  $ strip inputStr
