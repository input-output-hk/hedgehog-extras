{-# LANGUAGE MultiWayIf #-}
module Hedgehog.Extras.Test.Golden
  ( diffVsGoldenFile,
    diffFileVsGoldenFile,
  ) where

import           Control.Applicative
import           Control.Exception (bracket_)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Algorithm.Diff (PolyDiff (Both), getGroupedDiff)
import           Data.Algorithm.DiffOutput (ppDiff)
import           Data.Bool
import           Data.Eq
import           Data.Function
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           GHC.Stack (HasCallStack, callStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Test.Base (failMessage)
import           System.FilePath (takeDirectory)
import           System.IO (FilePath, IO)

import qualified Control.Concurrent.QSem as IO
import qualified Data.List as List
import qualified GHC.Stack as GHC
import qualified Hedgehog.Extras.Test as H
import qualified Hedgehog.Internal.Property as H
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

semBracket :: IO a -> IO a
semBracket = bracket_ (IO.waitQSem sem) (IO.signalQSem sem)

-- | The file to log whenever a golden file is referenced.
mGoldenFileLogFile :: Maybe FilePath
mGoldenFileLogFile = IO.unsafePerformIO $
  IO.lookupEnv "GOLDEN_FILE_LOG_FILE"

-- | Whether the test should create the golden files if the file does not exist.
createFiles :: Bool
createFiles = IO.unsafePerformIO $ do
  value <- IO.lookupEnv "CREATE_GOLDEN_FILES"
  return $ value == Just "1"

-- | Whether the test should recreate the golden files even if the files do exist.
recreateFiles :: Bool
recreateFiles = IO.unsafePerformIO $ do
  value <- IO.lookupEnv "RECREATE_GOLDEN_FILES"
  return $ value == Just "1"

-- | Diff contents against the golden file. If CREATE_GOLDEN_FILES environment is
-- set to "1", then should the gold file not exist it would be created. If
-- RECREATE_GOLDEN_FILES is set to "1", the golden files will be overwritten. If
-- GOLDEN_FILE_LOG_FILE is set to a filename, then the golden file path will be
-- logged to the specified file.
--
-- Set the environment variable when you intend to generate or re-generate the golden
-- file for example when running the test for the first time or if the golden file
-- genuinely needs to change.
--
-- To re-generate a golden file you must also delete the golden file because golden
-- files are never overwritten.
--
-- TODO: Improve the help output by saying the difference of each input.
diffVsGoldenFile
  :: HasCallStack
  => (MonadIO m, MonadTest m)
  => String   -- ^ Actual content
  -> FilePath -- ^ Reference file
  -> m ()
diffVsGoldenFile actualContent referenceFile = GHC.withFrozenCallStack $ do
  forM_ mGoldenFileLogFile $ \logFile ->
    liftIO $ semBracket $ IO.appendFile logFile $ referenceFile <> "\n"

  fileExists <- liftIO $ IO.doesFileExist referenceFile

  if
    | fileExists && not recreateFiles -> do
      referenceLines <- List.lines <$> H.readFile referenceFile
      let difference = getGroupedDiff actualLines referenceLines
      case difference of
        []       -> pure ()
        [Both{}] -> pure ()
        _        -> do
          H.note_ $ "Golden test failed against golden file: " <> referenceFile
          failMessage callStack $ ppDiff difference
    | ((not fileExists && createFiles) || recreateFiles) -> do
      -- CREATE_GOLDEN_FILES or RECREATE_GOLDEN_FILES is set, so we write all golden files
      H.note_ $ "Creating golden file " <> referenceFile
      H.createDirectoryIfMissing_ (takeDirectory referenceFile)
      H.writeFile referenceFile actualContent
    | otherwise -> do
      H.note_ $ mconcat
        [ "Golden file " <> referenceFile
        , " does not exist.  To create, run with CREATE_GOLDEN_FILES=1 or RECREATE_GOLDEN_FILES=1"
        ]
      H.failure
  where
    actualLines = List.lines actualContent

-- | Diff file against the golden file.  If CREATE_GOLDEN_FILES environment is
-- set to "1", then should the gold file not exist it would be created.  If
-- GOLDEN_FILE_LOG_FILE is set to a filename, then the golden file path will be
-- logged to the specified file.
--
-- Set the environment variable when you intend to generate or re-generate the golden
-- file for example when running the test for the first time or if the golden file
-- genuinely needs to change.
--
-- To re-generate a golden file you must also delete the golden file because golden
-- files are never overwritten.
diffFileVsGoldenFile
  :: HasCallStack
  => (MonadIO m, MonadTest m)
  => FilePath -- ^ Actual file
  -> FilePath -- ^ Reference file
  -> m ()
diffFileVsGoldenFile actualFile referenceFile = GHC.withFrozenCallStack $ do
  contents <- H.readFile actualFile
  diffVsGoldenFile contents referenceFile
