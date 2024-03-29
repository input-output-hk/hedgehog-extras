{-# LANGUAGE DeriveGeneric #-}

module Hedgehog.Extras.Stock.IO.Network.Sprocket
  ( Sprocket(..)
  , doesSprocketExist
  , sprocketArgumentName
  , sprocketSystemName
  , maxSprocketArgumentNameLength
  ) where

import           Data.Bool (Bool)
import           Data.Char (Char)
import           Data.Eq (Eq ((==)))
import           Data.Functor (Functor (fmap))
import           Data.Int (Int)
import           Data.Semigroup (Semigroup ((<>)))
import           Data.String (String)
import           GHC.Generics (Generic)
import           Hedgehog.Extras.Stock.OS (isWin32)
import           System.FilePath ((</>))
import           System.IO (FilePath, IO)
import           Text.Show (Show)

import qualified Hedgehog.Extras.Stock.IO.Network.NamedPipe as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO

-- | Socket emulation.  On Posix it represents a socket.  On Windows it represents a named pipe.
data Sprocket = Sprocket
  { sprocketBase :: String
  , sprocketName :: String
  } deriving (Generic, Eq, Show)

-- | Test if the sprocket exists
doesSprocketExist :: Sprocket -> IO Bool
doesSprocketExist socket = if isWin32
  then IO.doesNamedPipeExist (sprocketSystemName socket)
  else IO.doesSocketExist (sprocketSystemName socket)

-- | Use this to query the OS about the sprocket
sprocketSystemName :: Sprocket -> FilePath
sprocketSystemName sprocket@(Sprocket base name) = if isWin32
  then sprocketNamedPipeName sprocket
  else base </> name

-- | Use this when needing to pass a sprocket into a command line argument.
sprocketArgumentName :: Sprocket -> FilePath
sprocketArgumentName sprocket@(Sprocket _ name) = if isWin32
  then sprocketNamedPipeName sprocket
  else name

maxSprocketArgumentNameLength :: Int
maxSprocketArgumentNameLength = if isWin32
  then 256
  else 104

-- | The named pipe name of the sprocket on Win32 systems
sprocketNamedPipeName :: Sprocket -> FilePath
sprocketNamedPipeName (Sprocket _ name) = "\\\\.\\pipe" <> dedupBackslash ("\\" <> fmap slackToBack name)
  where slackToBack :: Char -> Char
        slackToBack c = if c == '/' then '\\' else c
        dedupBackslash :: String -> String
        dedupBackslash ('\\':'\\':xs) = dedupBackslash ('\\':xs)
        dedupBackslash (x:xs) = x:dedupBackslash xs
        dedupBackslash [] = []
