{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hedgehog.Extras.Test.Process
  ( createProcess
  , exec
  , exec_
  , execFlex
  , execFlex'
  , procFlex
  , binFlex

  , getProjectBase
  , waitForProcess
  , maybeWaitForProcess
  , getPid
  , waitSecondsForProcess

  , ExecConfig(..)
  , defaultExecConfig
  ) where

import           Control.Monad (Monad(..),  MonadFail(fail), void)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Resource (MonadResource, ReleaseKey, register)
import           Data.Aeson (eitherDecode)
import           Data.Bool (Bool(..))
import           Data.Either (Either(..))
import           Data.Eq (Eq(..))
import           Data.Function (($), (&), (.))
import           Data.Functor (Functor(..))
import           Data.Int (Int)
import           Data.Maybe (Maybe (..))
import           Data.Monoid (Last (..), mempty, (<>))
import           Data.String (String)
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Internal.Cli (argQuote)
import           Hedgehog.Extras.Internal.Plan (Component(..), Plan(..))
import           Hedgehog.Extras.Stock.IO.Process (TimedOut (..))
import           Prelude (error)
import           System.Exit (ExitCode)
import           System.FilePath (takeDirectory)
import           System.FilePath.Posix ((</>))
import           System.IO (FilePath, Handle, IO)
import           System.Process (CmdSpec (..), CreateProcess (..), Pid, ProcessHandle)
import           Text.Show (Show(show))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Text as T
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Process as IO
import qualified Hedgehog.Extras.Stock.OS as OS
import qualified Hedgehog.Extras.Test.Base as H
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.Exit as IO
import qualified System.IO.Unsafe as IO
import qualified System.Process as IO

-- | Configuration for starting a new process.  This is a subset of 'IO.CreateProcess'.
data ExecConfig = ExecConfig
  { execConfigEnv :: Last [(String, String)]
  , execConfigCwd :: Last FilePath
  } deriving (Eq, Show)

defaultExecConfig :: ExecConfig
defaultExecConfig = ExecConfig
  { execConfigEnv = mempty
  , execConfigCwd = mempty
  }

-- | Find the nearest plan.json going upwards from the current directory.
findDefaultPlanJsonFile :: IO FilePath
findDefaultPlanJsonFile = IO.getCurrentDirectory >>= go
  where go :: FilePath -> IO FilePath
        go d = do
          let file = d </> "dist-newstyle/cache/plan.json"
          exists <- IO.doesFileExist file
          if exists
            then return file
            else do
              let parent = takeDirectory d
              if parent == d
                then return "dist-newstyle/cache/plan.json"
                else go parent

-- | Discover the location of the plan.json file.
planJsonFile :: String
planJsonFile = IO.unsafePerformIO $ do
  maybeBuildDir <- liftIO $ IO.lookupEnv "CABAL_BUILDDIR"
  case maybeBuildDir of
    Just buildDir -> return $ ".." </> buildDir </> "cache/plan.json"
    Nothing -> findDefaultPlanJsonFile
{-# NOINLINE planJsonFile #-}

exeSuffix :: String
exeSuffix = if OS.isWin32 then ".exe" else ""

addExeSuffix :: String -> String
addExeSuffix s = if ".exe" `L.isSuffixOf` s
  then s
  else s <> exeSuffix

-- | Create a process returning handles to stdin, stdout, and stderr as well as the process handle.
createProcess
  :: (MonadTest m, MonadResource m, HasCallStack)
  => CreateProcess
  -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle, ReleaseKey)
createProcess cp = GHC.withFrozenCallStack $ do
  H.annotate $ "CWD: " <> show (IO.cwd cp)
  case IO.cmdspec cp of
    RawCommand cmd args -> H.annotate $ "Command line: " <> cmd <> " " <> L.unwords args
    ShellCommand cmd -> H.annotate $ "Command line: " <> cmd
  (mhStdin, mhStdout, mhStderr, hProcess) <- H.evalIO $ IO.createProcess cp
  releaseKey <- register $ IO.cleanupProcess (mhStdin, mhStdout, mhStderr, hProcess)

  return (mhStdin, mhStdout, mhStderr, hProcess, releaseKey)

-- | Get the process ID.
getPid
  :: (MonadTest m, MonadIO m, HasCallStack)
  => ProcessHandle
  -> m (Maybe Pid)
getPid hProcess = GHC.withFrozenCallStack . H.evalIO $ IO.getPid hProcess

-- | Create a process returning its stdout.
--
-- Being a 'flex' function means that the environment determines how the process is launched.
--
-- When running in a nix environment, the 'envBin' argument describes the environment variable
-- that defines the binary to use to launch the process.
--
-- When running outside a nix environment, the `pkgBin` describes the name of the binary
-- to launch via cabal exec.
execFlex
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => String
  -> String
  -> [String]
  -> m String
execFlex = execFlex' defaultExecConfig

execFlex'
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => ExecConfig
  -> String
  -> String
  -> [String]
  -> m String
execFlex' execConfig pkgBin envBin arguments = GHC.withFrozenCallStack $ do
  cp <- procFlex' execConfig pkgBin envBin arguments
  H.annotate . ("Command: " <>) $ case IO.cmdspec cp of
    IO.ShellCommand cmd -> cmd
    IO.RawCommand cmd args -> cmd <> " " <> L.unwords args
  (exitResult, stdout, stderr) <- H.evalIO $ IO.readCreateProcessWithExitCode cp ""
  case exitResult of
    IO.ExitFailure exitCode -> do
      H.annotate $ L.unlines $
        [ "Process exited with non-zero exit-code"
        , "━━━━ command ━━━━"
        , pkgBin <> " " <> L.unwords (fmap argQuote arguments)
        , "━━━━ stdout ━━━━"
        , stdout
        , "━━━━ stderr ━━━━"
        , stderr
        , "━━━━ exit code ━━━━"
        , show @Int exitCode
        ]
      H.failMessage GHC.callStack "Execute process failed"
    IO.ExitSuccess -> return stdout

-- | Execute a process, returning '()'.
exec_
  :: (MonadTest m, MonadIO m, HasCallStack)
  => ExecConfig
  -> String
  -> [String]
  -> m ()
exec_ execConfig bin arguments = void $ exec execConfig bin arguments

-- | Execute a process
exec
  :: (MonadTest m, MonadIO m, HasCallStack)
  => ExecConfig
  -> String
  -> [String]
  -> m String
exec execConfig bin arguments = GHC.withFrozenCallStack $ do
  let cp = (IO.proc bin arguments)
        { IO.env = getLast $ execConfigEnv execConfig
        , IO.cwd = getLast $ execConfigCwd execConfig
        }
  H.annotate . ("Command: " <>) $ bin <> " " <> L.unwords arguments
  (exitResult, stdout, stderr) <- H.evalIO $ IO.readCreateProcessWithExitCode cp ""
  case exitResult of
    IO.ExitFailure exitCode -> H.failMessage GHC.callStack . L.unlines $
      [ "Process exited with non-zero exit-code"
      , "━━━━ command ━━━━"
      , bin <> " " <> L.unwords (fmap argQuote arguments)
      , "━━━━ stdout ━━━━"
      , stdout
      , "━━━━ stderr ━━━━"
      , stderr
      , "━━━━ exit code ━━━━"
      , show @Int exitCode
      ]
    IO.ExitSuccess -> return stdout

-- | Wait for process to exit.
waitForProcess
  :: (MonadTest m, MonadIO m, HasCallStack)
  => ProcessHandle
  -> m ExitCode
waitForProcess hProcess = GHC.withFrozenCallStack $
  H.evalIO $ IO.waitForProcess hProcess

-- | Wait for process to exit or return 'Nothing' if interrupted by an asynchronous exception.
maybeWaitForProcess
  :: (MonadTest m, MonadIO m, HasCallStack)
  => ProcessHandle
  -> m (Maybe ExitCode)
maybeWaitForProcess hProcess = GHC.withFrozenCallStack $
  H.evalIO $ IO.maybeWaitForProcess hProcess

-- | Wait a maximum of 'seconds' secons for process to exit.
waitSecondsForProcess
  :: (MonadTest m, MonadIO m, HasCallStack)
  => Int
  -> ProcessHandle
  -> m (Either TimedOut ExitCode)
waitSecondsForProcess seconds hProcess = GHC.withFrozenCallStack $ do
  result <- H.evalIO $ IO.waitSecondsForProcess seconds hProcess
  case result of
    Left TimedOut -> do
      H.annotate "Timed out waiting for process to exit"
      return (Left TimedOut)
    Right maybeExitCode -> do
      case maybeExitCode of
        Nothing -> H.failMessage GHC.callStack "No exit code for process"
        Just exitCode -> do
          H.annotate $ "Process exited " <> show exitCode
          return (Right exitCode)

-- | Compute the path to the binary given a package name or an environment variable override.
binFlex
  :: (MonadTest m, MonadIO m)
  => String
  -- ^ Package name
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> m FilePath
  -- ^ Path to executable
binFlex pkg binaryEnv = do
  maybeEnvBin <- liftIO $ IO.lookupEnv binaryEnv
  case maybeEnvBin of
    Just envBin -> return envBin
    Nothing -> binDist pkg

-- | Consult the "plan.json" generated by cabal to get the path to the executable corresponding.
-- to a haskell package.  It is assumed that the project has already been configured and the
-- executable has been built.
binDist
  :: (MonadTest m, MonadIO m)
  => String
  -- ^ Package name
  -> m FilePath
  -- ^ Path to executable
binDist pkg = do
  contents <- H.evalIO . LBS.readFile $ planJsonFile

  case eitherDecode contents of
    Right plan -> case L.filter matching (plan & installPlan) of
      (component:_) -> case component & binFile of
        Just bin -> return $ addExeSuffix (T.unpack bin)
        Nothing -> error $ "missing bin-file in: " <> show component
      [] -> error $ "Cannot find exe:" <> pkg <> " in plan"
    Left message -> error $ "Cannot decode plan: " <> message
  where matching :: Component -> Bool
        matching component = case componentName component of
          Just name -> name == "exe:" <> T.pack pkg
          Nothing -> False

-- | Create a 'CreateProcess' describing how to start a process given the Cabal package name
-- corresponding to the executable, an environment variable pointing to the executable,
-- and an argument list.
--
-- The actual executable used will the one specified by the environment variable, but if
-- the environment variable is not defined, it will be found instead by consulting the
-- "plan.json" generated by cabal.  It is assumed that the project has already been
-- configured and the executable has been built.
procFlex
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procFlex = procFlex' defaultExecConfig

procFlex'
  :: (MonadTest m, MonadCatch m, MonadIO m, HasCallStack)
  => ExecConfig
  -> String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> m CreateProcess
  -- ^ Captured stdout
procFlex' execConfig pkg binaryEnv arguments = GHC.withFrozenCallStack . H.evalM $ do
  bin <- binFlex pkg binaryEnv
  return (IO.proc bin arguments)
    { IO.env = getLast $ execConfigEnv execConfig
    , IO.cwd = getLast $ execConfigCwd execConfig
    }

-- | Compute the project base.  This will be based on either the "CARDANO_NODE_SRC"
-- environment variable or the first parent directory that contains the `cabal.project`.
-- Both should point to the root directory of the Github project checkout.
getProjectBase
  :: (MonadTest m, MonadIO m)
  => m String
getProjectBase = do
  let
    findUp dir = do
      atBase <- liftIO $ IO.doesFileExist (dir <> "/cabal.project")
      if atBase
        then return dir
        else do
          let up = dir <> "/.."
          upExist <- liftIO $ IO.doesDirectoryExist up
          if upExist
            then findUp up
            else liftIO $ fail "Could not detect project base directory (containing cabal.project)"
  maybeNodeSrc <- liftIO $ IO.lookupEnv "CARDANO_NODE_SRC"
  case maybeNodeSrc of
    Just path -> return path
    Nothing -> findUp "."
