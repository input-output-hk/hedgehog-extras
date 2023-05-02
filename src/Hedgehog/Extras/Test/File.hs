{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Hedgehog.Extras.Test.File
  ( createDirectoryIfMissing
  , createDirectoryIfMissing_
  , createSubdirectoryIfMissing
  , createSubdirectoryIfMissing_
  , copyFile
  , renameFile
  , createFileLink
  , listDirectory

  , appendFile
  , writeFile
  , openFile
  , readFile
  , lbsWriteFile
  , lbsReadFile
  , textWriteFile
  , textReadFile

  , copyRewriteJsonFile
  , readJsonFile
  , readJsonFileOk
  , rewriteJsonFile
  , rewriteLbsJson

  , copyRewriteYamlFile
  , readYamlFile
  , readYamlFileOk
  , rewriteYamlFile
  , rewriteLbsYaml

  , cat

  , assertIsJsonFile
  , assertIsYamlFile

  , assertFileExists
  , assertFilesExist
  , assertFileMissing
  , assertFilesMissing
  , assertFileOccurences
  , assertFileLines
  , assertEndsWithSingleNewline

  , appendFileTimeDelta
  ) where

import           Control.Applicative (Applicative (..))
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson (Value)
import           Data.Bool
import           Data.Either
import           Data.Foldable (for_)
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Data.String (String)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Stock.Monad
import           Hedgehog.Extras.Stock.OS
import           System.FilePath ((</>))
import           System.IO (FilePath, Handle, IOMode)
import           Text.Show

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Text.IO as T
import qualified Data.Time.Clock as DTC
import qualified Data.Yaml as Y
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified System.Directory as IO
import qualified System.IO as IO

-- | Create the 'directory' directory if it is missing.
createDirectoryIfMissing :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m FilePath
createDirectoryIfMissing directory = GHC.withFrozenCallStack $ do
  H.annotate $ "Creating directory if missing: " <> directory
  H.evalIO $ IO.createDirectoryIfMissing True directory
  pure directory

-- | Create the 'directory' directory if it is missing.
createDirectoryIfMissing_ :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
createDirectoryIfMissing_ directory = GHC.withFrozenCallStack $
  void $ createDirectoryIfMissing directory

-- | Create the 'subdirectory' subdirectory if it is missing.  The subdirectory is returned.
createSubdirectoryIfMissing :: ()
  => HasCallStack
  => MonadTest m
  => MonadIO m
  => FilePath
  -> FilePath
  -> m FilePath
createSubdirectoryIfMissing parent subdirectory = GHC.withFrozenCallStack $ do
  H.annotate $ "Creating subdirectory if missing: " <> subdirectory
  H.evalIO $ IO.createDirectoryIfMissing True $ parent </> subdirectory
  pure subdirectory

-- | Create the 'subdirectory' subdirectory if it is missing.  The subdirectory is returned.
createSubdirectoryIfMissing_ :: ()
  => HasCallStack
  => MonadTest m
  => MonadIO m
  => FilePath
  -> FilePath
  -> m ()
createSubdirectoryIfMissing_ parent subdirectory = GHC.withFrozenCallStack $
  void $ createSubdirectoryIfMissing parent subdirectory

-- | Copy the contents of the 'src' file to the 'dst' file.
copyFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> FilePath -> m ()
copyFile src dst = GHC.withFrozenCallStack $ do
  H.annotate $ "Copying from " <> show src <> " to " <> show dst
  H.evalIO $ IO.copyFile src dst

-- | Rename the 'src' file to 'dst'.
renameFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> FilePath -> m ()
renameFile src dst = GHC.withFrozenCallStack $ do
  H.annotate $ "Copying from " <> show src <> " to " <> show dst
  H.evalIO $ IO.renameFile src dst

-- | Create a symbolic link from 'dst' to 'src'.
createFileLink :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> FilePath -> m ()
createFileLink src dst = GHC.withFrozenCallStack $ do
  H.annotate $ "Creating link from " <> show dst <> " to " <> show src
  if isWin32
    then H.evalIO $ IO.copyFile src dst
    else H.evalIO $ IO.createFileLink src dst

-- | List 'p' directory.
listDirectory :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m [FilePath]
listDirectory p = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Listing directory: " <> p
  H.evalIO $ IO.listDirectory p

-- | Append 'contents' to the 'filePath' file.
appendFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> String -> m ()
appendFile filePath contents = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Writing file: " <> filePath
  H.evalIO $ IO.appendFile filePath contents

-- | Write 'contents' to the 'filePath' file.
writeFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> String -> m ()
writeFile filePath contents = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Writing file: " <> filePath
  H.evalIO $ IO.writeFile filePath contents

-- | Open a handle to the 'filePath' file.
openFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> IOMode -> m Handle
openFile filePath mode = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Opening file: " <> filePath
  H.evalIO $ IO.openFile filePath mode

-- | Read the contents of the 'filePath' file.
readFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m String
readFile filePath = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Reading file: " <> filePath
  H.evalIO $ IO.readFile filePath

-- | Write 'contents' to the 'filePath' file.
lbsWriteFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> LBS.ByteString -> m ()
lbsWriteFile filePath contents = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Writing file: " <> filePath
  H.evalIO $ LBS.writeFile filePath contents

-- | Read the contents of the 'filePath' file.
lbsReadFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m LBS.ByteString
lbsReadFile filePath = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Reading file: " <> filePath
  H.evalIO $ LBS.readFile filePath

-- | Write 'contents' to the 'filePath' file.
textWriteFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> Text -> m ()
textWriteFile filePath contents = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Writing file: " <> filePath
  H.evalIO $ T.writeFile filePath contents

-- | Read the contents of the 'filePath' file.
textReadFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m Text
textReadFile filePath = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Reading file: " <> filePath
  H.evalIO $ T.readFile filePath

-- | Read the 'filePath' file as JSON.
readJsonFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m (Either String Value)
readJsonFile filePath = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Reading JSON file: " <> filePath
  H.evalIO $ J.eitherDecode @Value <$> LBS.readFile filePath

-- | Read the 'filePath' file as JSON. Same as 'readJsonFile' but fails on error.
readJsonFileOk :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m Value
readJsonFileOk filePath = GHC.withFrozenCallStack $
  H.leftFailM $ readJsonFile filePath

rewriteLbsJson :: (MonadTest m, HasCallStack) => (Value -> Value) -> LBS.ByteString -> m LBS.ByteString
rewriteLbsJson f lbs = GHC.withFrozenCallStack $ do
  case J.eitherDecode lbs of
    Right iv -> return (J.encode (f iv))
    Left msg -> H.failMessage GHC.callStack msg

-- | Rewrite the 'filePath' JSON file using the function 'f'.
rewriteJsonFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> (Value -> Value) -> m ()
rewriteJsonFile filePath f = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Rewriting JSON file: " <> filePath
  lbsReadFile filePath >>= rewriteLbsJson f >>= lbsWriteFile filePath

-- | Rewrite the 'filePath' JSON file using the function 'f'.
copyRewriteJsonFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> FilePath -> (Value -> Value) -> m ()
copyRewriteJsonFile src dst f = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Rewriting JSON from file: " <> src <> " to file " <> dst
  lbsReadFile src >>= rewriteLbsJson f >>= lbsWriteFile dst

-- | Read the 'filePath' file as YAML.
readYamlFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m (Either Y.ParseException Value)
readYamlFile filePath = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Reading YAML file: " <> filePath
  H.evalIO $ Y.decodeEither' @Value . LBS.toStrict <$> LBS.readFile filePath

-- | Read the 'filePath' file as YAML.  Same as 'readYamlFile' but fails on error.
readYamlFileOk :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m Value
readYamlFileOk filePath = GHC.withFrozenCallStack $
  H.leftFailM $ readYamlFile filePath

rewriteLbsYaml :: (MonadTest m, HasCallStack) => (Value -> Value) -> LBS.ByteString -> m LBS.ByteString
rewriteLbsYaml f lbs = GHC.withFrozenCallStack $ do
  case Y.decodeEither' (LBS.toStrict lbs) of
    Right iv -> return (J.encode (f iv))
    Left msg -> H.failMessage GHC.callStack (show msg)

-- | Rewrite the 'filePath' YAML file using the function 'f'.
rewriteYamlFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> (Value -> Value) -> m ()
rewriteYamlFile filePath f = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Rewriting YAML file: " <> filePath
  lbsReadFile filePath >>= rewriteLbsYaml f >>= lbsWriteFile filePath

-- | Rewrite the 'filePath' YAML file using the function 'f'.
copyRewriteYamlFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> FilePath -> (Value -> Value) -> m ()
copyRewriteYamlFile src dst f = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Rewriting YAML from file: " <> src <> " to file " <> dst
  lbsReadFile src >>= rewriteLbsYaml f >>= lbsWriteFile dst

-- | Annotate the contents of the 'filePath' file.
cat :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
cat filePath = GHC.withFrozenCallStack $ do
  !contents <- forceM $ readFile filePath
  void . H.annotate $ L.unlines
    [ "━━━━ File: " <> filePath <> " ━━━━"
    , contents
    ]
  return ()

-- | Assert the 'filePath' can be parsed as JSON.
assertIsJsonFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
assertIsJsonFile fp = GHC.withFrozenCallStack $ do
  jsonResult <- readJsonFile fp
  case jsonResult of
    Right _ -> return ()
    Left msg -> H.failMessage GHC.callStack msg

-- | Assert the 'filePath' can be parsed as YAML.
assertIsYamlFile :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
assertIsYamlFile fp = GHC.withFrozenCallStack $ do
  result <- readJsonFile fp
  case result of
    Right _ -> return ()
    Left msg -> H.failMessage GHC.callStack msg

-- | Asserts that the given file exists.
assertFileExists :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
assertFileExists file = GHC.withFrozenCallStack $ do
  exists <- H.evalIO $ IO.doesFileExist file
  unless exists $ H.failWithCustom GHC.callStack Nothing (file <> " has not been successfully created.")

-- | Asserts that all of the given files exist.
assertFilesExist :: (MonadTest m, MonadIO m, HasCallStack) => [FilePath] -> m ()
assertFilesExist files = GHC.withFrozenCallStack $ for_ files assertFileExists

-- | Asserts that the given file is missing.
assertFileMissing :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
assertFileMissing file = GHC.withFrozenCallStack $ do
  exists <- H.evalIO $ IO.doesFileExist file
  when exists $ H.failWithCustom GHC.callStack Nothing (file <> " should not have been created.")

-- | Asserts that all of the given files are missing.
assertFilesMissing :: (MonadTest m, MonadIO m, HasCallStack) => [FilePath] -> m ()
assertFilesMissing files = GHC.withFrozenCallStack $ for_ files assertFileMissing

-- | Assert the file contains the given number of occurrences of the given string
assertFileOccurences :: (MonadTest m, MonadIO m, HasCallStack) => Int -> String -> FilePath -> m ()
assertFileOccurences n s fp = GHC.withFrozenCallStack $ do
  contents <- readFile fp

  L.length (L.filter (s `L.isInfixOf`) (L.lines contents)) H.=== n

-- | Assert the file contains the given number of occurrences of the given string
assertFileLines :: (MonadTest m, MonadIO m, HasCallStack) => (Int -> Bool) -> FilePath -> m ()
assertFileLines p fp = GHC.withFrozenCallStack $ do
  contents <- readFile fp

  let lines = L.lines contents

  let len = case L.reverse lines of
        "":xs -> L.length xs
        xs -> L.length xs

  unless (p len) $ do
    H.failWithCustom GHC.callStack Nothing (fp <> " has an unexpected number of lines")

-- | Assert the file contains the given number of occurrences of the given string
assertEndsWithSingleNewline :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> m ()
assertEndsWithSingleNewline fp = GHC.withFrozenCallStack $ do
  contents <- readFile fp

  case L.reverse contents of
    '\n':'\n':_ -> H.failWithCustom GHC.callStack Nothing (fp <> " ends with too many newlines.")
    '\n':_ -> return ()
    _ -> H.failWithCustom GHC.callStack Nothing (fp <> " must end with newline.")

-- | Write 'contents' to the 'filePath' file.
appendFileTimeDelta :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> UTCTime ->  m ()
appendFileTimeDelta filePath offsetTime = GHC.withFrozenCallStack $ do
  baseTime <- H.noteShowIO DTC.getCurrentTime
  let delay = DTC.diffUTCTime baseTime offsetTime
  appendFile filePath $ show @DTC.NominalDiffTime delay <> "\n"
