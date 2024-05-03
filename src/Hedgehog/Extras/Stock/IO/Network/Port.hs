{-# LANGUAGE TypeApplications #-}

module Hedgehog.Extras.Stock.IO.Network.Port
  ( randomPort
  , reserveRandomPort
  , portInUse
  ) where

import           Control.Exception
import           Control.Monad (Monad (..), MonadFail (..))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bool
import           Data.Either
import           Data.Function
import           Network.Socket

-- | Return a random available port on a specified host address
randomPort :: ()
  => MonadIO m
  => MonadFail m
  => HostAddress
  -> m PortNumber
randomPort hostAddress = do
  sock <- liftIO $ socket AF_INET Stream defaultProtocol
  -- Allow the port to be reused immediately after the socket is closed
  liftIO $ setSocketOption sock ReuseAddr 1
  liftIO $ bind sock $ SockAddrInet defaultPort hostAddress
  SockAddrInet port _ <- liftIO $ getSocketName sock
  liftIO $ close sock
  return port

reserveRandomPort :: ()
  => MonadFail m
  => MonadResource m
  => HostAddress
  -> m (ReleaseKey, PortNumber)
reserveRandomPort hostAddress = do
  sock <- liftIO $ socket AF_INET Stream defaultProtocol
  liftIO $ setSocketOption sock ReuseAddr 1
  liftIO $ bind sock $ SockAddrInet defaultPort hostAddress
  SockAddrInet port _ <- liftIO $ getSocketName sock
  releaseKey <- register $ close sock
  return (releaseKey, port)

-- | Check if a port is in use on a specified host address
portInUse :: ()
  => MonadIO m
  => HostAddress
  -> PortNumber
  -> m Bool
portInUse hostAddress pn = do
  sock <- liftIO $ socket AF_INET Stream defaultProtocol
  liftIO $ setSocketOption sock ReuseAddr 1
  result <- liftIO $ try @SomeException $ bind sock (SockAddrInet pn hostAddress)
  liftIO $ close sock
  return $ either (const False) (const True) result
