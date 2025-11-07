{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hedgehog.Extras.Test.Process (
    createProcess,
    exec,
    execAny,
    exec_,
    execFlex,
    execFlex',
    execFlexAny',
    procFlex,
    binFlex,
    getProjectBase,
    waitForProcess,
    maybeWaitForProcess,
    getPid,
    getPidOk,
    waitSecondsForProcess,
    ExecConfig (..),
    defaultExecConfig,
) where

import Control.Monad (Monad (..), MonadFail (fail), void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, register)
import Data.Bool (Bool (True))
import Data.Either (Either (..))
import Data.Eq (Eq (..))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Monoid (Last (..), mempty, (<>))
import Data.String (String)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Hedgehog (MonadTest)
import Hedgehog.Extras.Internal.Cli (argQuote)
import qualified Hedgehog.Extras.Internal.Process as Internal
import Hedgehog.Extras.Stock.IO.Process (TimedOut (..))
import System.Exit (ExitCode)
import System.FilePath.Posix ((</>))
import System.IO (FilePath, Handle)
import System.Process (CmdSpec (..), CreateProcess (..), Pid, ProcessHandle)
import Text.Show (Show (show))
import Prelude ((++))

import qualified Data.List as L
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Stock.IO.Process as IO
import qualified Hedgehog.Extras.Test.Base as H
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.Exit as IO
import qualified System.Process as IO

-- | Configuration for starting a new process.  This is a subset of 'IO.CreateProcess'.
data ExecConfig = ExecConfig
    { execConfigEnv :: Last [(String, String)]
    , execConfigCwd :: Last FilePath
    }
    deriving (Eq, Generic, Show)

defaultExecConfig :: ExecConfig
defaultExecConfig =
    ExecConfig
        { execConfigEnv = mempty
        , execConfigCwd = mempty
        }

-- | Create a process returning handles to stdin, stdout, and stderr as well as the process handle.
createProcess ::
    (MonadTest m, MonadResource m, HasCallStack) =>
    CreateProcess ->
    m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle, ReleaseKey)
createProcess cp = GHC.withFrozenCallStack $ do
    H.annotate $ "CWD: " <> show (IO.cwd cp)
    case IO.cmdspec cp of
        RawCommand cmd args -> H.annotate $ "Command line: " <> cmd <> " " <> L.unwords args
        ShellCommand cmd -> H.annotate $ "Command line: " <> cmd
    (mhStdin, mhStdout, mhStderr, hProcess) <- H.evalIO $ IO.createProcess cp
    releaseKey <- register $ IO.cleanupProcess (mhStdin, mhStdout, mhStderr, hProcess)

    return (mhStdin, mhStdout, mhStderr, hProcess, releaseKey)

-- | Get the process ID.
getPid ::
    (MonadTest m, MonadIO m, HasCallStack) =>
    ProcessHandle ->
    m (Maybe Pid)
getPid hProcess = GHC.withFrozenCallStack . H.evalIO $ IO.getPid hProcess

-- | Get the process ID.
getPidOk ::
    (MonadTest m, MonadIO m, HasCallStack) =>
    ProcessHandle ->
    m Pid
getPidOk hProcess =
    GHC.withFrozenCallStack $
        H.nothingFailM $
            getPid hProcess

{- | Create a process returning its stdout.

Being a 'flex' function means that the environment determines how the process is launched.

When running in a nix environment, the 'envBin' argument describes the environment variable
that defines the binary to use to launch the process.

When running outside a nix environment, the `pkgBin` describes the name of the binary
to launch via cabal exec.
-}
execFlex ::
    (MonadTest m, MonadCatch m, MonadIO m, HasCallStack) =>
    String ->
    String ->
    [String] ->
    m String
execFlex = execFlex' defaultExecConfig

execFlex' ::
    (MonadTest m, MonadCatch m, MonadIO m, HasCallStack) =>
    ExecConfig ->
    String ->
    String ->
    [String] ->
    m String
execFlex' execConfig pkgBin envBin arguments = GHC.withFrozenCallStack $ do
    (exitResult, stdout, stderr) <- execFlexAny' execConfig pkgBin envBin arguments
    case exitResult of
        IO.ExitFailure exitCode -> do
            H.annotate $
                L.unlines $
                    ["Process exited with non-zero exit-code: " ++ show @Int exitCode]
                        ++ (if L.null stdout then [] else ["━━━━ stdout ━━━━", stdout])
                        ++ (if L.null stderr then [] else ["━━━━ stderr ━━━━", stderr])
            H.failMessage GHC.callStack "Execute process failed"
        IO.ExitSuccess -> return stdout

{- | Run a process, returning its exit code, its stdout, and its stderr.
Contrary to @execFlex'@, this function doesn't fail if the call fails.
So, if you want to test something negative, this is the function to use.
-}
execFlexAny' ::
    (MonadTest m, MonadCatch m, MonadIO m, HasCallStack) =>
    ExecConfig ->
    -- | @pkgBin@: name of the binary to launch via 'cabal exec'
    String ->
    -- | @envBin@: environment variable defining the binary to launch the process, when in Nix
    String ->
    [String] ->
    -- | exit code, stdout, stderr
    m (ExitCode, String, String)
execFlexAny' execConfig pkgBin envBin arguments = GHC.withFrozenCallStack $ do
    cp <- procFlex' execConfig pkgBin envBin arguments
    H.annotate . ("━━━━ command ━━━━\n" <>) $ case IO.cmdspec cp of
        IO.ShellCommand cmd -> cmd
        IO.RawCommand cmd args -> cmd <> " " <> L.unwords (argQuote <$> args)
    H.evalIO $ IO.readCreateProcessWithExitCode cp ""

-- | Execute a process, returning '()'.
exec_ ::
    (MonadTest m, MonadIO m, HasCallStack) =>
    ExecConfig ->
    String ->
    [String] ->
    m ()
exec_ execConfig bin arguments = void $ exec execConfig bin arguments

{- | Execute a process, returning the stdout. Fail if the call returns
with a non-zero exit code. For a version that doesn't fail upon receiving
a non-zero exit code, see 'execAny'.
-}
exec ::
    (MonadTest m, MonadIO m, HasCallStack) =>
    ExecConfig ->
    String ->
    [String] ->
    m String
exec execConfig bin arguments = GHC.withFrozenCallStack $ do
    (exitResult, stdout, stderr) <- execAny execConfig bin arguments
    case exitResult of
        IO.ExitFailure exitCode ->
            H.failMessage GHC.callStack . L.unlines $
                ["Process exited with non-zero exit-code: " ++ show @Int exitCode]
                    ++ (if L.null stdout then [] else ["━━━━ stdout ━━━━", stdout])
                    ++ (if L.null stderr then [] else ["━━━━ stderr ━━━━", stderr])
        IO.ExitSuccess -> return stdout

-- | Execute a process, returning the error code, the stdout, and the stderr.
execAny ::
    (MonadTest m, MonadIO m, HasCallStack) =>
    ExecConfig ->
    -- | The binary to launch
    String ->
    -- | The binary's arguments
    [String] ->
    -- | exit code, stdout, stderr
    m (ExitCode, String, String)
execAny execConfig bin arguments = GHC.withFrozenCallStack $ do
    let cp =
            (IO.proc bin arguments)
                { IO.env = getLast $ execConfigEnv execConfig
                , IO.cwd = getLast $ execConfigCwd execConfig
                }
    H.annotate . ("━━━━ command ━━━━\n" <>) $ bin <> " " <> L.unwords (argQuote <$> arguments)
    H.evalIO $ IO.readCreateProcessWithExitCode cp ""

-- | Wait for process to exit.
waitForProcess ::
    (MonadTest m, MonadIO m, HasCallStack) =>
    ProcessHandle ->
    m ExitCode
waitForProcess hProcess =
    GHC.withFrozenCallStack $
        H.evalIO $
            IO.waitForProcess hProcess

-- | Wait for process to exit or return 'Nothing' if interrupted by an asynchronous exception.
maybeWaitForProcess ::
    (MonadTest m, MonadIO m, HasCallStack) =>
    ProcessHandle ->
    m (Maybe ExitCode)
maybeWaitForProcess hProcess =
    GHC.withFrozenCallStack $
        H.evalIO $
            IO.maybeWaitForProcess hProcess

-- | Wait a maximum of 'seconds' secons for process to exit.
waitSecondsForProcess ::
    (MonadTest m, MonadIO m, HasCallStack) =>
    Int ->
    ProcessHandle ->
    m (Either TimedOut ExitCode)
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
binFlex ::
    (HasCallStack, MonadTest m, MonadIO m) =>
    -- | Package name
    String ->
    -- | Environment variable pointing to the binary to run
    String ->
    -- | Path to executable
    m FilePath
binFlex pkg binaryEnv = do
    maybeEnvBin <- liftIO $ IO.lookupEnv binaryEnv
    case maybeEnvBin of
        Just envBin -> return envBin
        Nothing -> Internal.binDist pkg binaryEnv

{- | Create a 'CreateProcess' describing how to start a process given the Cabal package name
corresponding to the executable, an environment variable pointing to the executable,
and an argument list.

The actual executable used will the one specified by the environment variable, but if
the environment variable is not defined, it will be found instead by consulting the
"plan.json" generated by cabal.  It is assumed that the project has already been
configured and the executable has been built.
-}
procFlex ::
    (MonadTest m, MonadCatch m, MonadIO m, HasCallStack) =>
    -- | Cabal package name corresponding to the executable
    String ->
    -- | Environment variable pointing to the binary to run
    String ->
    -- | Arguments to the CLI command
    [String] ->
    -- | Captured stdout
    m CreateProcess
procFlex = procFlex' defaultExecConfig

procFlex' ::
    (MonadTest m, MonadCatch m, MonadIO m, HasCallStack) =>
    ExecConfig ->
    -- | Cabal package name corresponding to the executable
    String ->
    -- | Environment variable pointing to the binary to run
    String ->
    -- | Arguments to the CLI command
    [String] ->
    -- | Captured stdout
    m CreateProcess
procFlex' execConfig pkg binaryEnv arguments = GHC.withFrozenCallStack . H.evalM $ do
    bin <- binFlex pkg binaryEnv
    return
        (IO.proc bin arguments)
            { IO.env = getLast $ execConfigEnv execConfig
            , IO.cwd = getLast $ execConfigCwd execConfig
            , -- this allows sending signals to the created processes, without killing the test-suite process
              IO.create_group = True
            }

{- | Compute the project base.  This will be based on either the "CARDANO_NODE_SRC"
environment variable or the first parent directory that contains the `cabal.project`.
Both should point to the root directory of the Github project checkout.
-}
getProjectBase ::
    (MonadTest m, MonadIO m) =>
    m String
getProjectBase = do
    let
        findUp dir = do
            atBase <- liftIO $ IO.doesFileExist (dir </> "cabal.project")
            if atBase
                then return dir
                else do
                    let up = dir </> ".."
                    upExist <- liftIO $ IO.doesDirectoryExist up
                    if upExist
                        then findUp up
                        else liftIO $ fail "Could not detect project base directory (containing cabal.project)"
    maybeNodeSrc <- liftIO $ IO.lookupEnv "CARDANO_NODE_SRC"
    case maybeNodeSrc of
        Just path -> return path
        Nothing -> findUp "."
