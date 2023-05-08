{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Redundant flip" -}

module Hedgehog.Extras.Test.Network
  ( doesFileExists
  , isPortOpen
  , doesSocketExist
  , assertPortOpen
  , assertSocketExists
  , doesSprocketExist
  , downloadToFile
  , downloadAndExtractGithubCommitToTemp
  ) where

import           Control.Exception (IOException, try)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Data.Bool
import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.Semigroup
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket, sprocketSystemName)
import           Prelude (String)
import           System.FilePath ((</>))
import           System.IO (FilePath)
import           Text.Show

import qualified Codec.Archive.Tar as TAR
import qualified Codec.Archive.Tar.Check as TAR
import qualified Codec.Compression.GZip as GZ
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Network.NamedPipe as IO
import qualified Hedgehog.Extras.Stock.IO.Network.Socket as IO
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Test.Base as H
import qualified Network.HTTP.Conduit as HTTP
import qualified System.Directory as H
import qualified System.Directory as IO
import qualified System.FilePath as FP

-- | Test if a file exists
doesFileExists :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m Bool
doesFileExists = GHC.withFrozenCallStack . H.evalIO . IO.doesFileExist

-- | Test if a port is open
isPortOpen :: (MonadTest m, MonadIO m, HasCallStack) => Int -> m Bool
isPortOpen port = GHC.withFrozenCallStack $ do
  H.note_ $ "Port: " <> show port
  H.evalIO $ IO.isPortOpen port

-- | Test if a socket file exists
doesSocketExist :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m Bool
doesSocketExist = GHC.withFrozenCallStack . H.evalIO . IO.doesSocketExist

-- | Assert that a port is open
assertPortOpen :: (MonadTest m, MonadIO m, HasCallStack) => Int -> m ()
assertPortOpen = GHC.withFrozenCallStack . H.assertM . isPortOpen

-- | Assert that a socket file exists is open
assertSocketExists :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
assertSocketExists = GHC.withFrozenCallStack . H.assertM . doesSocketExist

-- | Test if the sprocket exists
doesSprocketExist :: (MonadTest m, MonadIO m, HasCallStack) => Sprocket -> m Bool
doesSprocketExist socket = GHC.withFrozenCallStack $ do
  waitResult <- H.evalIO . try $ if OS.isWin32
    then IO.doesNamedPipeExist (sprocketSystemName socket)
    else IO.doesSocketExist (sprocketSystemName socket)
  case waitResult of
    Right result -> return result
    Left (e :: IOException) -> do
      H.annotate $ "Error: " <> show e
      return False

-- | Download from a URl to a file
downloadToFile :: (MonadTest m, MonadIO m, HasCallStack) => String -> FilePath -> m ()
downloadToFile url path = GHC.withFrozenCallStack $ do
  H.note_ $ "Downloading " <> url <> " to " <> path
  H.evalIO $ HTTP.simpleHttp url >>= LBS.writeFile path

tarErrors :: TAR.Entries (Either TAR.FormatError TAR.TarBombError) -> [Either TAR.FormatError TAR.TarBombError]
tarErrors entries = TAR.foldEntries (flip const) id (:) entries []

-- | Download a github commit to a temporary directory, extract it and return the path to the extracted directory.
--
-- If the file is already downloaded, it will not be downloaded again.
-- If the file is already extracted, it will not be extracted again.
downloadAndExtractGithubCommitToTemp :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> String -> String -> m FilePath
downloadAndExtractGithubCommitToTemp dir repository commit = GHC.withFrozenCallStack $ do
  let url = "https://github.com/" <> repository <> "/archive/" <> commit <> ".tar.gz"
  let topDir = FP.takeFileName repository <> "-" <> commit
  let tarPath = dir </> topDir <> ".tar.gz"
  let dest = dir </> topDir

  tarFileExists <- H.evalIO $ IO.doesFileExist tarPath
  if tarFileExists
    then H.note_ $ "Already downloaded " <> url <> " to " <> tarPath
    else do
      H.note_ $ "Downloading " <> url <> " to " <> tarPath
      H.evalIO $ HTTP.simpleHttp url >>= LBS.writeFile tarPath

  destExists <- H.evalIO $ IO.doesDirectoryExist dest
  if destExists
    then H.note_ $ "Already extracted " <> tarPath <> " to " <> dest
    else do
      H.note_ $ "Extracting " <> tarPath <> " to " <> dest
      errors <- H.evalIO $ tarErrors . TAR.checkTarbomb topDir . TAR.read . GZ.decompress <$> LBS.readFile tarPath

      unless (List.null errors) $ do
        H.annotate $ "Errors: " <> show errors
        H.failure

      H.evalIO $ TAR.unpack dir . TAR.read . GZ.decompress =<< LBS.readFile tarPath

      void . H.assertIO $ H.doesDirectoryExist dest

  H.note dest
