{-# LANGUAGE BangPatterns #-}
module Hedgehog.Extras.Test.WorkspaceSpec where

import Control.Applicative
import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Data.Bool
import Data.Either
import Data.Function (($))
import Data.IORef
import Data.Maybe
import Data.Semigroup ((<>))
import GHC.Err (error)
import Hedgehog
import Hedgehog.Extras.Test.Base
import Hedgehog.Extras.Test.Unit
import System.IO.Error (userError)
import Text.Show

import qualified System.Directory as IO
import qualified Hedgehog.Internal.Property as H

-- | Test that workspace directories are removed on successful completion when keepWorkspace=False
tasty_workspace_removed_on_success_on_keepWorkspace_False :: UnitIO ()
tasty_workspace_removed_on_success_on_keepWorkspace_False = do
  workspacePath <- liftIO $ newIORef Nothing
  
  -- This workspace operation will succeed
  workspaceWithConfig CleanupOnSuccess "test-success" $ \ws -> do
    liftIO $ writeIORef workspacePath (Just ws)
    -- Successful operation - no failing assertions
    True === True
  
  -- After successful completion, workspace should be removed
  maybePath <- liftIO $ readIORef workspacePath
  case maybePath of
    Nothing -> do
      H.failWith Nothing "Expected workspace path to be recorded, but got Nothing"
    Just path -> do
      exists <- liftIO $ IO.doesDirectoryExist path
      annotate $ "Workspace path: " <> path
      annotate $ "Directory exists after success: " <> show exists
      -- On success, directory should be removed
      exists === False

-- | Test that workspace directories are preserved when assertion fails (keepWorkspace=False)
tasty_workspace_kept_on_assertion_on_keepWorkspace_False :: UnitIO ()
tasty_workspace_kept_on_assertion_on_keepWorkspace_False = do
  -- This test will intentionally trigger an assertion failure to verify
  -- that the workspace directory is preserved for debugging
  workspacePath <- liftIO $ newIORef Nothing
  
  result <- tryAssertion $ workspaceWithConfig CleanupOnSuccess "test-failure" $ \ws -> do
    -- Store the workspace path so we can check it later
    liftIO $ writeIORef workspacePath (Just ws)
    -- Intentionally trigger assertion failure
    False === True
  
  case result of
    Left _ -> do
      -- Assertion failed as expected, now check if workspace was correctly preserved
      maybePath <- liftIO $ readIORef workspacePath
      case maybePath of
        Nothing -> do
          H.failWith Nothing "Expected workspace path to be recorded after failed assertion, but got Nothing"
        Just path -> do
          exists <- liftIO $ IO.doesDirectoryExist path
          -- Correct behavior: directory is preserved on assertion failure for debugging
          annotate $ "Workspace path: " <> path
          annotate $ "Directory exists: " <> show exists
          -- Directory should be preserved when assertion fails
          exists === True
          -- Clean up manually since assertion failed and we preserved it
          liftIO $ IO.removeDirectoryRecursive path
    Right _ -> do
      -- Assertion unexpectedly passed, this shouldn't happen
      annotate "Test was supposed to fail but didn't"
      failure

-- | Test that workspace directories are preserved when pure exception occurs (keepWorkspace=False)
tasty_workspace_kept_on_pure_exception_on_keepWorkspace_False :: UnitIO ()
tasty_workspace_kept_on_pure_exception_on_keepWorkspace_False = do
  workspacePath <- liftIO $ newIORef Nothing
  
  result <- tryAssertion $ workspaceWithConfig CleanupOnSuccess "test-pure-exception" $ \ws -> do
    -- Store the workspace path so we can check it later
    liftIO $ writeIORef workspacePath (Just ws)
    -- Force evaluation of pure code that throws an exception
    let !_ = error "Pure code exception in workspace" :: ()
    pure ()
  
  case result of
    Left _ -> do
      -- Exception was thrown as expected, now check if workspace was correctly preserved
      maybePath <- liftIO $ readIORef workspacePath
      case maybePath of
        Nothing ->
          H.failWith Nothing "Expected workspace path to be recorded after pure exception, but got Nothing"
        Just path -> do
          exists <- liftIO $ IO.doesDirectoryExist path
          annotate $ "Workspace path after pure exception: " <> path
          annotate $ "Directory exists after pure exception: " <> show exists
          -- Correct behavior: directory is preserved when pure code throws exception
          exists === True
          -- Clean up manually since exception occurred and we preserved it
          liftIO $ IO.removeDirectoryRecursive path
    Right _ -> do
      -- Pure code unexpectedly didn't throw exception
      annotate "Pure code was supposed to throw exception but didn't"
      failure

-- | Test that workspace directories are preserved when IO exception occurs (keepWorkspace=False)
tasty_workspace_kept_on_io_exception_on_keepWorkspace_False :: UnitIO ()
tasty_workspace_kept_on_io_exception_on_keepWorkspace_False = do
  workspacePath <- liftIO $ newIORef Nothing
  
  result <- tryAssertion $ workspaceWithConfig CleanupOnSuccess "test-io-exception" $ \ws -> do
    -- Store the workspace path so we can check it later
    liftIO $ writeIORef workspacePath (Just ws)
    -- Throw an IO exception within the workspace
    liftIO $ throwIO (userError "IO exception in workspace")
  
  case result of
    Left _ -> do
      -- Exception was thrown as expected, now check if workspace was correctly preserved
      maybePath <- liftIO $ readIORef workspacePath
      case maybePath of
        Nothing ->
          H.failWith Nothing "Expected workspace path to be recorded after IO exception, but got Nothing"
        Just path -> do
          exists <- liftIO $ IO.doesDirectoryExist path
          annotate $ "Workspace path after IO exception: " <> path
          annotate $ "Directory exists after IO exception: " <> show exists
          -- Correct behavior: directory is preserved when IO exception is thrown
          exists === True
          -- Clean up manually since exception occurred and we preserved it
          liftIO $ IO.removeDirectoryRecursive path
    Right _ -> do
      -- IO operation unexpectedly didn't throw exception
      annotate "IO operation was supposed to throw exception but didn't"
      failure

-- | Test that workspace directories are preserved on successful completion when keepWorkspace=True
tasty_workspace_kept_on_success_on_keepWorkspace_True :: UnitIO ()
tasty_workspace_kept_on_success_on_keepWorkspace_True = do
  workspacePath <- liftIO $ newIORef Nothing
  
  -- This workspace operation will succeed
  workspaceWithConfig PreserveWorkspace "test-keep-success" $ \ws -> do
    liftIO $ writeIORef workspacePath (Just ws)
    -- Successful operation - no failing assertions
    True === True
  
  -- After successful completion, workspace should still be preserved due to keepWorkspace=True
  maybePath <- liftIO $ readIORef workspacePath
  case maybePath of
    Nothing -> do
      H.failWith Nothing "Expected workspace path to be recorded with keepWorkspace=True, but got Nothing"
    Just path -> do
      exists <- liftIO $ IO.doesDirectoryExist path
      annotate $ "Workspace path with keepWorkspace=True (success): " <> path
      annotate $ "Directory exists after success with keepWorkspace=True: " <> show exists
      -- With keepWorkspace=True, directory should be preserved even on success
      exists === True
      -- Clean up manually since we kept it
      liftIO $ IO.removeDirectoryRecursive path

-- | Test that workspace directories are preserved when assertion fails (keepWorkspace=True)
tasty_workspace_kept_on_assertion_on_keepWorkspace_True :: UnitIO ()
tasty_workspace_kept_on_assertion_on_keepWorkspace_True = do
  workspacePath <- liftIO $ newIORef Nothing
  
  result <- tryAssertion $ workspaceWithConfig PreserveWorkspace "test-keep" $ \ws -> do
    liftIO $ writeIORef workspacePath (Just ws)
    -- This assertion will fail, but workspace should be kept due to keepWorkspace=True
    False === True
  
  case result of
    Left _ -> do
      maybePath <- liftIO $ readIORef workspacePath
      case maybePath of
        Nothing -> do
          H.failWith Nothing "Expected workspace path to be recorded with keepWorkspace=True after failed assertion, but got Nothing"
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

-- | Test that workspace directories are preserved when pure exception occurs (keepWorkspace=True)
tasty_workspace_kept_on_pure_exception_on_keepWorkspace_True :: UnitIO ()
tasty_workspace_kept_on_pure_exception_on_keepWorkspace_True = do
  workspacePath <- liftIO $ newIORef Nothing
  
  result <- tryAssertion $ workspaceWithConfig PreserveWorkspace "test-keep-pure-exception" $ \ws -> do
    -- Store the workspace path so we can check it later
    liftIO $ writeIORef workspacePath (Just ws)
    -- Force evaluation of pure code that throws an exception
    let !_ = error "Pure code exception with keepWorkspace=True" :: ()
    pure ()
  
  case result of
    Left _ -> do
      -- Exception was thrown as expected, now check if workspace was correctly preserved
      maybePath <- liftIO $ readIORef workspacePath
      case maybePath of
        Nothing -> do
          H.failWith Nothing "Expected workspace path to be recorded with keepWorkspace=True after pure exception, but got Nothing"
        Just path -> do
          exists <- liftIO $ IO.doesDirectoryExist path
          annotate $ "Workspace path after pure exception with keepWorkspace=True: " <> path
          annotate $ "Directory exists after pure exception with keepWorkspace=True: " <> show exists
          -- With keepWorkspace=True, directory should always be preserved
          exists === True
          -- Clean up manually since we kept it
          liftIO $ IO.removeDirectoryRecursive path
    Right _ -> do
      -- Pure code unexpectedly didn't throw exception
      annotate "Pure code was supposed to throw exception but didn't"
      failure

-- | Test that workspace directories are preserved when IO exception occurs (keepWorkspace=True)
tasty_workspace_kept_on_io_exception_on_keepWorkspace_True :: UnitIO ()
tasty_workspace_kept_on_io_exception_on_keepWorkspace_True = do
  workspacePath <- liftIO $ newIORef Nothing
  
  result <- tryAssertion $ workspaceWithConfig PreserveWorkspace "test-keep-io-exception" $ \ws -> do
    -- Store the workspace path so we can check it later
    liftIO $ writeIORef workspacePath (Just ws)
    -- Throw an IO exception within the workspace
    liftIO $ throwIO (userError "IO exception with keepWorkspace=True")
  
  case result of
    Left _ -> do
      -- Exception was thrown as expected, now check if workspace was correctly preserved
      maybePath <- liftIO $ readIORef workspacePath
      case maybePath of
        Nothing -> do
          H.failWith Nothing "Expected workspace path to be recorded with keepWorkspace=True after IO exception, but got Nothing"
        Just path -> do
          exists <- liftIO $ IO.doesDirectoryExist path
          annotate $ "Workspace path after IO exception with keepWorkspace=True: " <> path
          annotate $ "Directory exists after IO exception with keepWorkspace=True: " <> show exists
          -- With keepWorkspace=True, directory should always be preserved
          exists === True
          -- Clean up manually since we kept it
          liftIO $ IO.removeDirectoryRecursive path
    Right _ -> do
      -- IO operation unexpectedly didn't throw exception
      annotate "IO operation was supposed to throw exception but didn't"
      failure
