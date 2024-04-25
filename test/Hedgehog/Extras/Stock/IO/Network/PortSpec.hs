module Hedgehog.Extras.Stock.IO.Network.PortSpec
  ( hprop_randomPort
  ) where

import           Data.Function
import           Data.Semigroup
import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Port as IO
import qualified Network.Socket as N
import           Text.Show
import qualified Data.Time as D
import Control.Monad.IO.Class
import Control.Applicative

hprop_randomPort :: Property
hprop_randomPort =
  H.propertyOnce $ do
    let hostAddress = N.tupleToHostAddress (0, 0, 0, 0)

    pn <- H.evalIO $ IO.randomPort hostAddress

    H.note_ $ "Allocated port: " <> show pn

    -- retry binding for 5 seconds - seems that sometimes OS still marks port as unavailable for a while
    -- after 'randomPort' call
    deadline <- D.addUTCTime 5 <$> liftIO D.getCurrentTime
    H.byDeadlineM 0.2 deadline "try binding to allocated port" $ do
      -- Check that the port is available and can be bound to a socket.
      sock <- H.evalIO $ N.socket N.AF_INET N.Stream N.defaultProtocol
      H.evalIO $ N.bind sock $ N.SockAddrInet pn hostAddress
      H.evalIO $ N.close sock
