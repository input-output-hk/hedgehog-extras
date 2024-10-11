{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Extras.Test.Base
  ( propertyOnce

  , workspace
  , moduleWorkspace

  , note
  , note_
  , noteM
  , noteM_
  , noteIO
  , noteIO_

  , noteShow
  , noteShowPretty
  , noteShowIO
  , noteShowPrettyIO
  , noteShowIO_
  , noteShowPrettyIO_
  , noteShowM
  , noteShowPrettyM
  , noteShowM_
  , noteShowPrettyM_
  , noteShow_
  , noteShowPretty_

  , noteEach
  , noteEachPretty
  , noteEachIO
  , noteEachPrettyIO
  , noteEachIO_
  , noteEachPrettyIO_
  , noteEachM
  , noteEachPrettyM
  , noteEachM_
  , noteEachPrettyM_
  , noteEach_
  , noteEachPretty_

  , noteTempFile

  , headM
  , indexM
  , fromJustM

  , nothingFail
  , nothingFailM
  , leftFail
  , leftFailM

  , onLeft
  , onNothing

  , jsonErrorFail
  , jsonErrorFailM

  , failWithCustom
  , failMessage

  , assertByDeadlineM
  , assertByDeadlineIO
  , assertByDeadlineMFinally
  , assertByDeadlineIOFinally
  , assertWith
  , assertWithM
  , assertM
  , assertIO
  , assertWithinTolerance

  , byDeadlineM
  , byDeadlineIO
  , byDurationM
  , byDurationIO

  , onFailure

  , Integration
  , release

  , runFinallies

  , retry
  , retry'
  ) where

import           Control.Applicative (Applicative (..))
import           Control.Monad (Functor (fmap), Monad (return, (>>=)), mapM_, unless, void, when)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Morph (hoist)
import           Control.Monad.Reader (MonadIO (..), MonadReader (ask))
import           Control.Monad.Trans.Resource (ReleaseKey, runResourceT)
import           Data.Aeson (Result (..))
import           Data.Bool (Bool, (&&), otherwise)
import           Data.Either (Either (..), either)
import           Data.Eq (Eq ((/=)))
import           Data.Foldable (for_)
import           Data.Function (const, ($), (.))
import           Data.Functor ((<$>))
import           Data.Int (Int)
import           Data.Maybe (Maybe (..), listToMaybe, maybe)
import           Data.Monoid (Monoid (..))
import           Data.Semigroup (Semigroup (..))
import           Data.String (String)
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import           Data.Traversable (Traversable)
import           Data.Tuple (snd)
import           GHC.Stack (CallStack, HasCallStack)
import           Hedgehog (MonadTest)
import           Hedgehog.Extras.Internal.Test.Integration (Integration, IntegrationState (..))
import           Hedgehog.Extras.Stock.CallStack (callerModuleName)
import           Hedgehog.Extras.Stock.Monad (forceM)
import           Hedgehog.Extras.Test.MonadAssertion (MonadAssertion)
import           Hedgehog.Internal.Property (Diff, liftTest, mkTest)
import           Hedgehog.Internal.Source (getCaller)
import           Prelude (Num (..), Ord (..), floor)
import           System.FilePath ((</>))
import           System.IO (FilePath, IO)
import           Text.Show (Show (show))

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Trans.Resource as IO
import qualified Data.List as L
import qualified Data.Time.Clock as DTC
import qualified GHC.Stack as GHC
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Internal.Test.Integration as H
import qualified Hedgehog.Extras.Test.MonadAssertion as H
import qualified Hedgehog.Internal.Property as H
import qualified Hedgehog.Internal.Show as H
import qualified System.Directory as IO
import qualified System.Environment as IO
import qualified System.Info as IO
import qualified System.IO as IO
import qualified System.IO.Temp as IO

{- HLINT ignore "Reduce duplication" -}

-- | Run a property with only one test.  This is intended for allowing hedgehog
-- to run unit tests.
propertyOnce :: HasCallStack => Integration () -> H.Property
propertyOnce = H.withTests 1 . H.property . hoist runResourceT . hoist H.runIntegrationReaderT

-- | Takes a 'CallStack' so the error can be rendered at the appropriate call site.
failWithCustom :: MonadTest m => CallStack -> Maybe Diff -> String -> m a
failWithCustom cs mdiff msg = liftTest $ mkTest (Left $ H.Failure (getCaller cs) msg mdiff, mempty)

-- | Takes a 'CallStack' so the error can be rendered at the appropriate call site.
failMessage :: MonadTest m => CallStack -> String -> m a
failMessage cs = failWithCustom cs Nothing

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the supplied prefix but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
workspace :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> (FilePath -> m ()) -> m ()
workspace prefixPath f = GHC.withFrozenCallStack $ do
  systemTemp <- H.evalIO IO.getCanonicalTemporaryDirectory
  maybeKeepWorkspace <- H.evalIO $ IO.lookupEnv "KEEP_WORKSPACE"
  ws <- H.evalIO $ IO.createTempDirectory systemTemp $ prefixPath <> "-test"
  H.annotate $ "Workspace: " <> ws
  H.evalIO $ IO.writeFile (ws </> "module") callerModuleName
  f ws
  when (IO.os /= "mingw32" && maybeKeepWorkspace /= Just "1") $ do
    H.evalIO $ IO.removePathForcibly ws

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the prefix as "$prefixPath/$moduleName" but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
--
-- The 'prefix' argument should not contain directory delimeters.
moduleWorkspace :: (MonadTest m, MonadIO m, HasCallStack) => String -> (FilePath -> m ()) -> m ()
moduleWorkspace prefix f = GHC.withFrozenCallStack $ do
  let srcModule = maybe "UnknownModule" (GHC.srcLocModule . snd) (listToMaybe (GHC.getCallStack GHC.callStack))
  workspace (prefix <> "-" <> srcModule) f

-- | Annotate the given string at the context supplied by the callstack.
noteWithCallstack :: MonadTest m => CallStack -> String -> m ()
noteWithCallstack cs a = H.writeLog $ H.Annotation (getCaller cs) a

-- | Annotate with the given string.
note :: (MonadTest m, HasCallStack) => String -> m String
note a = GHC.withFrozenCallStack $ do
  !b <- H.eval a
  noteWithCallstack GHC.callStack b
  return b

-- | Annotate the given string returning unit.
note_ :: (MonadTest m, HasCallStack) => String -> m ()
note_ a = GHC.withFrozenCallStack $ noteWithCallstack GHC.callStack a

-- | Annotate the given string in a monadic context.
noteM :: (MonadTest m, MonadCatch m, HasCallStack) => m String -> m String
noteM a = GHC.withFrozenCallStack $ do
  !b <- H.evalM a
  noteWithCallstack GHC.callStack b
  return b

-- | Annotate the given string in a monadic context returning unit.
noteM_ :: (MonadTest m, MonadCatch m, HasCallStack) => m String -> m ()
noteM_ a = GHC.withFrozenCallStack $ do
  !b <- H.evalM a
  noteWithCallstack GHC.callStack b
  return ()

-- | Annotate the given string in IO.
noteIO :: (MonadTest m, MonadIO m, HasCallStack) => IO String -> m String
noteIO f = GHC.withFrozenCallStack $ do
  !a <- H.evalIO f
  noteWithCallstack GHC.callStack a
  return a

-- | Annotate the given string in IO returning unit.
noteIO_ :: (MonadTest m, MonadIO m, HasCallStack) => IO String -> m ()
noteIO_ f = GHC.withFrozenCallStack $ do
  !a <- H.evalIO f
  noteWithCallstack GHC.callStack a
  return ()

-- | Annotate the given value.
noteShow :: (MonadTest m, HasCallStack, Show a) => a -> m a
noteShow a = GHC.withFrozenCallStack $ do
  !b <- H.eval a
  noteWithCallstack GHC.callStack (show b)
  return b

-- | Annotate the given value, pretty printing it with indentation. Note that large data structures will take
-- a significant amount of vertical screen space.
noteShowPretty :: (MonadTest m, HasCallStack, Show a) => a -> m a
noteShowPretty a = GHC.withFrozenCallStack $ do
  !b <- H.eval a
  noteWithCallstack GHC.callStack (H.showPretty b)
  return b

-- | Annotate the given value returning unit.
noteShow_ :: (MonadTest m, HasCallStack, Show a) => a -> m ()
noteShow_ a = GHC.withFrozenCallStack $ noteWithCallstack GHC.callStack (show a)

-- | Annotate the given value returning unit, pretty printing it with indentation. Note that large data structures will take
-- a significant amount of vertical screen space.
noteShowPretty_ :: (MonadTest m, HasCallStack, Show a) => a -> m ()
noteShowPretty_ a = GHC.withFrozenCallStack $ noteWithCallstack GHC.callStack (H.showPretty a)

-- | Annotate the given value in a monadic context.
noteShowM :: (MonadTest m, MonadCatch m, HasCallStack, Show a) => m a -> m a
noteShowM a = GHC.withFrozenCallStack $ do
  !b <- H.evalM a
  noteWithCallstack GHC.callStack (show b)
  return b

-- | Annotate the given value in a monadic context, pretty printing it with indentation. Note that large data structures will take
-- a significant amount of vertical screen space.
noteShowPrettyM :: (MonadTest m, MonadCatch m, HasCallStack, Show a) => m a -> m a
noteShowPrettyM a = GHC.withFrozenCallStack $ do
  !b <- H.evalM a
  noteWithCallstack GHC.callStack (H.showPretty b)
  return b

-- | Annotate the given value in a monadic context returning unit.
noteShowM_ :: (MonadTest m, MonadCatch m, HasCallStack, Show a) => m a -> m ()
noteShowM_ a = GHC.withFrozenCallStack $ do
  !b <- H.evalM a
  noteWithCallstack GHC.callStack (show b)
  return ()

-- | Annotate the given value in a monadic context returning unit, pretty printing it with indentation. Note that large data structures will take
-- a significant amount of vertical screen space.
noteShowPrettyM_ :: (MonadTest m, MonadCatch m, HasCallStack, Show a) => m a -> m ()
noteShowPrettyM_ a = GHC.withFrozenCallStack $ do
  !b <- H.evalM a
  noteWithCallstack GHC.callStack (H.showPretty b)
  return ()

-- | Annotate the given value in IO.
noteShowIO :: (MonadTest m, MonadIO m, HasCallStack, Show a) => IO a -> m a
noteShowIO f = GHC.withFrozenCallStack $ do
  !a <- H.evalIO f
  noteWithCallstack GHC.callStack (show a)
  return a

-- | Annotate the given value in IO, pretty printing it with indentation. Note that large data structures will take
-- a significant amount of vertical screen space.
noteShowPrettyIO :: (MonadTest m, MonadIO m, HasCallStack, Show a) => IO a -> m a
noteShowPrettyIO f = GHC.withFrozenCallStack $ do
  !a <- H.evalIO f
  noteWithCallstack GHC.callStack (H.showPretty a)
  return a

-- | Annotate the given value in IO returning unit.
noteShowIO_ :: (MonadTest m, MonadIO m, HasCallStack, Show a) => IO a -> m ()
noteShowIO_ f = GHC.withFrozenCallStack $ do
  !a <- H.evalIO f
  noteWithCallstack GHC.callStack (show a)
  return ()

-- | Annotate the given value in IO returning unit, pretty printing it with indentation. Note that large data structures will take
-- a significant amount of vertical screen space.
noteShowPrettyIO_ :: (MonadTest m, MonadIO m, HasCallStack, Show a) => IO a -> m ()
noteShowPrettyIO_ f = GHC.withFrozenCallStack $ do
  !a <- H.evalIO f
  noteWithCallstack GHC.callStack (H.showPretty a)
  return ()

-- | Annotate the each value in the given traversable.
noteEach :: (MonadTest m, HasCallStack, Show a, Traversable f) => f a -> m (f a)
noteEach as = GHC.withFrozenCallStack $ do
  for_ as $ noteWithCallstack GHC.callStack . show
  return as

-- | Annotate the each value in the given traversable, pretty printing it with indentation. Note that large data structures will take
-- a significant amount of vertical screen space.
noteEachPretty :: (MonadTest m, HasCallStack, Show a, Traversable f) => f a -> m (f a)
noteEachPretty as = GHC.withFrozenCallStack $ do
  for_ as $ noteWithCallstack GHC.callStack . H.showPretty
  return as

-- | Annotate the each value in the given traversable returning unit.
noteEach_ :: (MonadTest m, HasCallStack, Show a, Traversable f) => f a -> m ()
noteEach_ as = GHC.withFrozenCallStack $ for_ as $ noteWithCallstack GHC.callStack . show

-- | Annotate the each value in the given traversable returning unit, pretty printing it with indentation. Note that large data structures will take
-- a significant amount of vertical screen space.
noteEachPretty_ :: (MonadTest m, HasCallStack, Show a, Traversable f) => f a -> m ()
noteEachPretty_ as = GHC.withFrozenCallStack $ for_ as $ noteWithCallstack GHC.callStack . H.showPretty

-- | Annotate the each value in the given traversable in a monadic context.
noteEachM :: (MonadTest m, HasCallStack, Show a, Traversable f) => m (f a) -> m (f a)
noteEachM f = GHC.withFrozenCallStack $ do
  !as <- f
  for_ as $ noteWithCallstack GHC.callStack . show
  return as

-- | Annotate the each value in the given traversable in a monadic context, pretty printing it with indentation. Note that large data structures will take
-- a significant amount of vertical screen space.
noteEachPrettyM :: (MonadTest m, HasCallStack, Show a, Traversable f) => m (f a) -> m (f a)
noteEachPrettyM f = GHC.withFrozenCallStack $ do
  !as <- f
  for_ as $ noteWithCallstack GHC.callStack . H.showPretty
  return as

-- | Annotate the each value in the given traversable in a monadic context returning unit.
noteEachM_ :: (MonadTest m, HasCallStack, Show a, Traversable f) => m (f a) -> m ()
noteEachM_ f = GHC.withFrozenCallStack $ do
  !as <- f
  for_ as $ noteWithCallstack GHC.callStack . show

-- | Annotate the each value in the given traversable in a monadic context returning unit, pretty printing it with indentation. Note that large data structures will take
-- a significant amount of vertical screen space.
noteEachPrettyM_ :: (MonadTest m, HasCallStack, Show a, Traversable f) => m (f a) -> m ()
noteEachPrettyM_ f = GHC.withFrozenCallStack $ do
  !as <- f
  for_ as $ noteWithCallstack GHC.callStack . H.showPretty

-- | Annotate the each value in the given traversable in IO.
noteEachIO :: (MonadTest m, MonadIO m, HasCallStack, Show a, Traversable f) => IO (f a) -> m (f a)
noteEachIO f = GHC.withFrozenCallStack $ do
  !as <- H.evalIO f
  for_ as $ noteWithCallstack GHC.callStack . show
  return as

-- | Annotate the each value in the given traversable in IO, pretty printing it with indentation. Note that large data structures will take
-- a significant amount of vertical screen space.
noteEachPrettyIO :: (MonadTest m, MonadIO m, HasCallStack, Show a, Traversable f) => IO (f a) -> m (f a)
noteEachPrettyIO f = GHC.withFrozenCallStack $ do
  !as <- H.evalIO f
  for_ as $ noteWithCallstack GHC.callStack . H.showPretty
  return as

-- | Annotate the each value in the given traversable in IO returning unit.
noteEachIO_ :: (MonadTest m, MonadIO m, HasCallStack, Show a, Traversable f) => IO (f a) -> m ()
noteEachIO_ f = GHC.withFrozenCallStack $ do
  !as <- H.evalIO f
  for_ as $ noteWithCallstack GHC.callStack . show

-- | Annotate the each value in the given traversable in IO returning unit, pretty printing it with indentation. Note that large data structures will take
-- a significant amount of vertical screen space.
noteEachPrettyIO_ :: (MonadTest m, MonadIO m, HasCallStack, Show a, Traversable f) => IO (f a) -> m ()
noteEachPrettyIO_ f = GHC.withFrozenCallStack $ do
  !as <- H.evalIO f
  for_ as $ noteWithCallstack GHC.callStack . H.showPretty

-- | Return the test file path after annotating it relative to the project root directory
noteTempFile :: (MonadTest m, HasCallStack) => FilePath -> FilePath -> m FilePath
noteTempFile tempDir filePath = GHC.withFrozenCallStack $ do
  let relPath = tempDir </> filePath
  H.annotate relPath
  return relPath

-- | Fail when the result is Nothing.
nothingFail :: (MonadTest m, HasCallStack) => Maybe a -> m a
nothingFail r = GHC.withFrozenCallStack $ case r of
  Just a -> return a
  Nothing -> failMessage GHC.callStack "Expected Just"

-- | Fail when the computed result is Nothing.
nothingFailM :: (MonadTest m, HasCallStack) => m (Maybe a) -> m a
nothingFailM f = GHC.withFrozenCallStack $ f >>= nothingFail

-- | Fail when the result is Left.
leftFail :: (MonadTest m, Show e, HasCallStack) => Either e a -> m a
leftFail r = GHC.withFrozenCallStack $ case r of
  Right a -> return a
  Left e -> failMessage GHC.callStack ("Expected Right: " <> show e)

-- | Fail when the computed result is Left.
leftFailM :: (MonadTest m, Show e, HasCallStack) => m (Either e a) -> m a
leftFailM f = GHC.withFrozenCallStack $ f >>= leftFail

maybeAt :: Int -> [a] -> Maybe a
maybeAt n xs
  | n < 0 = Nothing
  | otherwise = L.foldr go (const Nothing) xs n
      where
        go :: a -> (Int -> Maybe a) -> Int -> Maybe a
        go x r k =
          case k of
            0 -> Just x
            _ -> r (k - 1)

headM :: (MonadTest m, HasCallStack) => [a] -> m a
headM (a:_) = return a
headM [] = GHC.withFrozenCallStack $ failMessage GHC.callStack "Cannot take head of empty list"

indexM :: (MonadTest m, HasCallStack) => Int -> [a] -> m a
indexM n xs =
  case maybeAt n xs of
    Just x -> pure x
    Nothing ->
      GHC.withFrozenCallStack $
        failMessage GHC.callStack $ "Cannot get index " <> show n <> " of list of length " <> show (L.length xs)

onLeft :: Monad m => (e -> m a) -> m (Either e a) -> m a
onLeft h f = f >>= either h pure

onNothing :: Monad m => m a -> m (Maybe a) -> m a
onNothing h f = f >>= maybe h pure

-- | Index into a list.  On failure, a friendly message is included in the test report.
fromJustM :: (MonadTest m, HasCallStack) => Maybe a -> m a
fromJustM (Just a) = return a
fromJustM Nothing = GHC.withFrozenCallStack $ failMessage GHC.callStack "Cannot take head of empty list"

-- | Fail when the result is Error.
jsonErrorFail :: (MonadTest m, HasCallStack) => Result a -> m a
jsonErrorFail r = GHC.withFrozenCallStack $ case r of
  Success a -> return a
  Error msg -> failMessage GHC.callStack ("Expected Right: " <> msg)

-- | Fail when the computed result is Error.
jsonErrorFailM :: (MonadTest m, HasCallStack) => m (Result a) -> m a
jsonErrorFailM f = GHC.withFrozenCallStack $ f >>= jsonErrorFail

-- | Run the operation 'f' once a second until it returns 'True' or the deadline expires.
--
-- Expiration of the deadline results in an assertion failure
byDeadlineIO :: (MonadAssertion m, MonadTest m, MonadIO m, HasCallStack) => NominalDiffTime -> UTCTime -> String -> IO a -> m a
byDeadlineIO period deadline errorMessage f = GHC.withFrozenCallStack $ byDeadlineM period deadline errorMessage $ liftIO f

-- | Run the operation 'f' once a second until it returns 'True' or the deadline expires.
--
-- Expiration of the deadline results in an assertion failure
byDeadlineM :: forall m a. (MonadAssertion m, MonadTest m, MonadIO m, HasCallStack) => NominalDiffTime -> UTCTime -> String -> m a -> m a
byDeadlineM period deadline errorMessage f = GHC.withFrozenCallStack $ do
  start <- liftIO DTC.getCurrentTime
  a <- goM
  end <- liftIO DTC.getCurrentTime
  note_ $ "Operation completed in " <> show (DTC.diffUTCTime end start)
  return a
  where goM :: m a
        goM = H.catchAssertion f $ \e -> do
          currentTime <- liftIO DTC.getCurrentTime
          if currentTime < deadline
            then do
              liftIO $ IO.threadDelay (floor (DTC.nominalDiffTimeToSeconds period * 1000000))
              goM
            else do
              H.annotateShow currentTime
              void $ failMessage GHC.callStack $ "Condition not met by deadline: " <> errorMessage
              H.throwAssertion e

-- | Run the operation 'f' once a second until it returns 'True' or the duration expires.
--
-- Expiration of the duration results in an assertion failure
byDurationIO :: (MonadAssertion m, MonadTest m, MonadIO m, HasCallStack) => NominalDiffTime -> NominalDiffTime -> String -> IO a -> m a
byDurationIO period duration errorMessage f = GHC.withFrozenCallStack $ byDurationM period duration errorMessage $ liftIO f

-- | Run the operation 'f' once a second until it returns 'True' or the duration expires.
--
-- Expiration of the duration results in an assertion failure
byDurationM :: (MonadAssertion m, MonadTest m, MonadIO m, HasCallStack) => NominalDiffTime -> NominalDiffTime -> String -> m a -> m a
byDurationM period duration errorMessage f = GHC.withFrozenCallStack $ do
  deadline <- DTC.addUTCTime duration <$> liftIO DTC.getCurrentTime
  byDeadlineM period deadline errorMessage f

-- | Run the operation 'f' once a second until it returns 'True' or the deadline expires.
--
-- Expiration of the deadline results in an assertion failure
assertByDeadlineIO :: (MonadTest m, MonadIO m, HasCallStack) => UTCTime -> IO Bool -> m ()
assertByDeadlineIO deadline f = GHC.withFrozenCallStack $ do
  success <- liftIO f
  unless success $ do
    currentTime <- liftIO DTC.getCurrentTime
    if currentTime < deadline
      then do
        liftIO $ IO.threadDelay 1000000
        assertByDeadlineIO deadline f
      else do
        H.annotateShow currentTime
        failMessage GHC.callStack "Condition not met by deadline"

-- | Run the operation 'f' once a second until it returns 'True' or the deadline expires.
--
-- Expiration of the deadline results in an assertion failure
assertByDeadlineM :: (MonadTest m, MonadIO m, HasCallStack) => UTCTime -> m Bool -> m ()
assertByDeadlineM deadline f = GHC.withFrozenCallStack $ do
  success <- f
  unless success $ do
    currentTime <- liftIO DTC.getCurrentTime
    if currentTime < deadline
      then do
        liftIO $ IO.threadDelay 1000000
        assertByDeadlineM deadline f
      else do
        H.annotateShow currentTime
        failMessage GHC.callStack "Condition not met by deadline"

-- | Run the operation 'f' once a second until it returns 'True' or the deadline expires.
--
-- The action 'g' is run after expiration of the deadline, but before failure allowing for
-- additional annotations to be presented.
--
-- Expiration of the deadline results in an assertion failure
assertByDeadlineIOFinally :: (MonadTest m, MonadIO m, HasCallStack) => UTCTime -> IO Bool -> m () -> m ()
assertByDeadlineIOFinally deadline f g = GHC.withFrozenCallStack $ do
  success <- liftIO f
  unless success $ do
    currentTime <- liftIO DTC.getCurrentTime
    if currentTime < deadline
      then do
        liftIO $ IO.threadDelay 1000000
        assertByDeadlineIOFinally deadline f g
      else do
        H.annotateShow currentTime
        g
        failMessage GHC.callStack "Condition not met by deadline"

-- | Run the operation 'f' once a second until it returns 'True' or the deadline expires.
--
-- The action 'g' is run after expiration of the deadline, but before failure allowing for
-- additional annotations to be presented.
--
-- Expiration of the deadline results in an assertion failure
assertByDeadlineMFinally :: (MonadTest m, MonadIO m, HasCallStack) => UTCTime -> m Bool -> m () -> m ()
assertByDeadlineMFinally deadline f g = GHC.withFrozenCallStack $ do
  success <- f
  unless success $ do
    currentTime <- liftIO DTC.getCurrentTime
    if currentTime < deadline
      then do
        liftIO $ IO.threadDelay 1000000
        assertByDeadlineMFinally deadline f g
      else do
        H.annotateShow currentTime
        g
        failMessage GHC.callStack "Condition not met by deadline"

-- | Run the test function against the value. Report the value on the failure.
assertWith :: (H.MonadTest m, Show p, HasCallStack) => p -> (p -> Bool) -> m ()
assertWith v f = GHC.withFrozenCallStack $ assertWithM v (pure . f)

-- | Run the test function against the value. Report the value on the failure.
assertWithM :: (H.MonadTest m, Show p, HasCallStack) => p -> (p -> m Bool) -> m ()
assertWithM v f = GHC.withFrozenCallStack $ do
  result <- f v
  if result
     then H.success
     else do
       noteShow_ v
       H.assert result

-- | Run the monadic action 'f' and assert the return value is 'True'.
assertM :: (MonadTest m, HasCallStack) => m Bool -> m ()
assertM f = GHC.withFrozenCallStack $ f >>= H.assert

-- | Run the IO action 'f' and assert the return value is 'True'.
assertIO :: (MonadTest m, MonadIO m, HasCallStack) => IO Bool -> m ()
assertIO f = GHC.withFrozenCallStack $ H.evalIO (forceM f) >>= H.assert

-- | Tests if @|c - v| <= r@
assertWithinTolerance :: (Show a, Ord a, Num a, HasCallStack, H.MonadTest m)
                  => a -- ^ tested value @v@
                  -> a -- ^ expected value @c@
                  -> a -- ^ tolerance range @r@
                  -> m ()
assertWithinTolerance v c r = GHC.withFrozenCallStack $ do
  H.diff v (>=) (c - r)
  H.diff v (<=) (c + r)

-- | Release the given release key.
release :: (MonadTest m, MonadIO m) => ReleaseKey -> m ()
release k = GHC.withFrozenCallStack . H.evalIO $ IO.release k

onFailure :: Integration () -> Integration ()
onFailure f = do
  s <- ask
  liftIO . STM.atomically $ STM.modifyTVar (integrationStateFinals s) (f:)

reportFinally :: Integration () -> Integration ()
reportFinally f = do
  result <- H.catchAssertion (fmap Right f) (return . Left)

  case result of
    Right () -> return ()
    Left a -> note_ $ "Unable to run finally: " <> show a

runFinallies :: Integration a -> Integration a
runFinallies f = do
  result <- H.catchAssertion (fmap Right f) (return . Left)

  case result of
    Right a -> return a
    Left assertion -> do
      s <- ask
      finals <- liftIO . STM.atomically $ STM.swapTVar (integrationStateFinals s) []
      mapM_ reportFinally finals
      H.throwAssertion assertion

retry :: forall a. Int -> (Int -> Integration a) -> Integration a
retry n f = go 0
  where go :: Int -> Integration a
        go i = do
          note_ $ "Retry attempt " <> show i <> " of " <> show n
          result <- H.catchAssertion (fmap Right (f i)) (return . Left)

          case result of
            Right a -> return a
            Left assertion -> do
              if i < n
                then go (i + 1)
                else do
                  note_ $ "All " <> show n <> " attempts failed"
                  H.throwAssertion assertion

retry' :: forall a. Int -> Integration a -> Integration a
retry' n f = retry n (const f)
