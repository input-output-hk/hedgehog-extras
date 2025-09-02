module Hedgehog.Extras.Test.WorkspaceSpec where

import Control.Monad.IO.Class
import Data.Bool
import Data.Either
import Data.Function (($))
import Data.IORef
import Data.Maybe
import Data.Semigroup ((<>))
import Hedgehog
import Hedgehog.Extras.Test.Base
import Hedgehog.Extras.Test.Unit
import Text.Show

import qualified System.Directory as IO

-- Test that demonstrates the current broken behavior:
-- workspace directories are removed even when tests fail
tasty_workspace_removed_on_failure :: UnitIO ()
tasty_workspace_removed_on_failure = do
  -- This test will intentionally fail, but we want to verify that
  -- the workspace directory is incorrectly removed despite the failure
  workspacePath <- liftIO $ newIORef Nothing
  
  result <- tryAssertion $ workspaceWithConfig False "test-failure" $ \ws -> do
    -- Store the workspace path so we can check it later
    liftIO $ writeIORef workspacePath (Just ws)
    -- Intentionally fail the test
    False === True
  
  case result of
    Left _ -> do
      -- Test failed as expected, now check if workspace was incorrectly removed
      maybePath <- liftIO $ readIORef workspacePath
      case maybePath of
        Nothing -> failure
        Just path -> do
          exists <- liftIO $ IO.doesDirectoryExist path
          -- Current broken behavior: directory is removed even on failure due to bracket
          -- This test documents the current wrong behavior
          annotate $ "Workspace path: " <> path
          annotate $ "Directory exists: " <> show exists
          -- Current implementation with bracket always removes directory
          -- This is WRONG - should be `exists === True` after fix
          exists === False
    Right _ -> do
      -- Test unexpectedly passed, this shouldn't happen
      annotate "Test was supposed to fail but didn't"
      failure

-- Test that workspace directories are removed on successful completion
tasty_workspace_removed_on_success :: UnitIO ()
tasty_workspace_removed_on_success = do
  workspacePath <- liftIO $ newIORef Nothing
  
  -- This workspace operation will succeed
  workspaceWithConfig False "test-success" $ \ws -> do
    liftIO $ writeIORef workspacePath (Just ws)
    -- Successful operation - no failing assertions
    True === True
  
  -- After successful completion, workspace should be removed
  maybePath <- liftIO $ readIORef workspacePath
  case maybePath of
    Nothing -> failure
    Just path -> do
      exists <- liftIO $ IO.doesDirectoryExist path
      annotate $ "Workspace path: " <> path
      annotate $ "Directory exists after success: " <> show exists
      -- On success, directory should be removed
      exists === False

-- Test that workspace directories are preserved when keepWorkspace=True
tasty_workspace_always_kept :: UnitIO ()
tasty_workspace_always_kept = do
  workspacePath <- liftIO $ newIORef Nothing
  
  result <- tryAssertion $ workspaceWithConfig True "test-keep" $ \ws -> do
    liftIO $ writeIORef workspacePath (Just ws)
    -- This test will fail, but workspace should be kept due to keepWorkspace=True
    False === True
  
  case result of
    Left _ -> do
      maybePath <- liftIO $ readIORef workspacePath
      case maybePath of
        Nothing -> failure
        Just path -> do
          exists <- liftIO $ IO.doesDirectoryExist path
          annotate $ "Workspace path: " <> path
          annotate $ "Directory exists with keepWorkspace=True: " <> show exists
          -- With keepWorkspace=True, directory should always be preserved
          exists === True
          -- Clean up manually since we kept it
          liftIO $ IO.removeDirectoryRecursive path
    Right _ -> do
      annotate "Test was supposed to fail but didn't"
      failure