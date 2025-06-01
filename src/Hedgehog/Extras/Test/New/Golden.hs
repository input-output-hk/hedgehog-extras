{-# LANGUAGE MultiWayIf #-}

module Hedgehog.Extras.Test.New.Golden
  ( diffVsGoldenFile,
    diffFileVsGoldenFile,
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
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
import           System.IO (FilePath)

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

semBracket :: ()
  => MonadIO m
  => MonadMask m
  => m a
  -> m a
semBracket =
  bracket_
    (liftIO (IO.waitQSem sem))
    (liftIO (IO.signalQSem sem))

-- | The file to log whenever a golden file is referenced.
mGoldenFileLogFile :: Maybe FilePath
mGoldenFileLogFile = IO.unsafePerformIO $
  IO.lookupEnv "GOLDEN_FILE_LOG_FILE"

-- | Whether the test should create the golden files if the files do not exist.
createGoldenFiles :: Bool
createGoldenFiles = IO.unsafePerformIO $ do
  value <- IO.lookupEnv "CREATE_GOLDEN_FILES"
  return $ value == Just "1"

-- | Whether the test should recreate the golden files if the files already exist.
recreateGoldenFiles :: Bool
recreateGoldenFiles = IO.unsafePerformIO $ do
  value <- IO.lookupEnv "RECREATE_GOLDEN_FILES"
  return $ value == Just "1"

writeGoldenFile :: ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => FilePath
  -> String
  -> m ()
writeGoldenFile goldenFile actualContent = GHC.withFrozenCallStack $ do
  H.note_ $ "Creating golden file " <> goldenFile
  H.createDirectoryIfMissing_ (takeDirectory goldenFile)
  H.writeFile goldenFile actualContent

reportGoldenFileMissing :: ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => FilePath
  -> m ()
reportGoldenFileMissing goldenFile = GHC.withFrozenCallStack $ do
  H.note_ $ unlines
    [ "Golden file " <> goldenFile <> " does not exist."
    , "To create it, run with CREATE_GOLDEN_FILES=1."
    , "To recreate it, run with RECREATE_GOLDEN_FILES=1."
    ]
  H.failure

checkAgainstGoldenFile :: ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => FilePath
  -> [String]
  -> m ()
checkAgainstGoldenFile goldenFile actualLines = GHC.withFrozenCallStack $ do
  referenceLines <- List.lines <$> H.readFile goldenFile
  let difference = getGroupedDiff actualLines referenceLines
  case difference of
    []       -> pure ()
    [Both{}] -> pure ()
    _        -> do
      H.note_ $ unlines
        [ "Golden test failed against the golden file."
        , "To recreate golden file, run with RECREATE_GOLDEN_FILES=1."
        ]
      failMessage callStack $ ppDiff difference

-- | Diff contents against the golden file.  If CREATE_GOLDEN_FILES environment is
-- set to "1", then should the golden file not exist it would be created.  If
-- RECREATE_GOLDEN_FILES is set to "1", then should the golden file exist it would
-- be recreated. If GOLDEN_FILE_LOG_FILE is set to a filename, then the golden file
-- path will be logged to the specified file.
--
-- Set the environment variable when you intend to generate or re-generate the golden
-- file for example when running the test for the first time or if the golden file
-- genuinely needs to change.
--
-- To re-generate a golden file you must also delete the golden file because golden
-- files are never overwritten.
--
-- TODO: Improve the help output by saying the difference of
-- each input.
diffVsGoldenFile
  :: HasCallStack
  => MonadIO m
  => MonadMask m
  => MonadTest m
  => String   -- ^ Actual content
  -> FilePath -- ^ Reference file
  -> m ()
diffVsGoldenFile actualContent goldenFile =
  GHC.withFrozenCallStack $ semBracket $ do
    forM_ mGoldenFileLogFile $ \logFile ->
      liftIO $ IO.appendFile logFile $ goldenFile <> "\n"

    fileExists <- liftIO $ IO.doesFileExist goldenFile

    if
      | recreateGoldenFiles -> writeGoldenFile goldenFile actualContent
      | fileExists          -> checkAgainstGoldenFile goldenFile actualLines
      | createGoldenFiles   -> writeGoldenFile goldenFile actualContent
      | otherwise           -> reportGoldenFileMissing goldenFile

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
  => MonadIO m
  => MonadMask m
  => MonadTest m
  => FilePath -- ^ Actual file
  -> FilePath -- ^ Reference file
  -> m ()
diffFileVsGoldenFile actualFile referenceFile = GHC.withFrozenCallStack $ do
  contents <- H.readFile actualFile
  diffVsGoldenFile contents referenceFile
