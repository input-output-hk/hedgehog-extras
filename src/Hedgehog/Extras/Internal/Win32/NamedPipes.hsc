#include <fcntl.h>
#include <windows.h>

{-# LANGUAGE CPP                #-}
{-# LANGUAGE MultiWayIf         #-}

-- | For full details on the Windows named pipes API see
-- <https://docs.microsoft.com/en-us/windows/desktop/ipc/named-pipes>
--
module Hedgehog.Extras.Internal.Win32.NamedPipes
  ( -- ** waiting for named pipe instances
    waitNamedPipe,

    TimeOut,
    nMPWAIT_USE_DEFAULT_WAIT,
    nMPWAIT_WAIT_FOREVER,
  ) where

import Control.Applicative (pure)
import Data.Bool (Bool (..), otherwise)
import Data.Eq ((==))
import Data.Function (($))
import Data.String (String)
import Foreign.C.String (withCString)
import System.IO (IO)
import System.Win32.Types hiding (try)

-- | Timeout in milliseconds.
--
-- * 'nMPWAIT_USE_DEFAULT_WAIT' indicates that the timeout value passed to
--   'createNamedPipe' should be used.
-- * 'nMPWAIT_WAIT_FOREVER' - 'waitNamedPipe' will block forever, until a named
--   pipe instance is available.
--
type TimeOut = DWORD
#{enum TimeOut,
 , nMPWAIT_USE_DEFAULT_WAIT = NMPWAIT_USE_DEFAULT_WAIT
 , nMPWAIT_WAIT_FOREVER     = NMPWAIT_WAIT_FOREVER
 }

-- | Wait until a named pipe instance is available.  If there is no instance at
-- hand before the timeout, it will error with 'ERROR_SEM_TIMEOUT', i.e.
-- @invalid argument (The semaphore timeout period has expired)@
--
-- It returns 'True' if there is an available instance, subsequent 'createFile'
-- might still fail, if another thread will take turn and connect before, or if
-- the other end shuts down the name pipe.
--
-- It returns 'False' if timeout fired.
--
waitNamedPipe :: String  -- ^ pipe name
              -> TimeOut -- ^ nTimeOut
              -> IO Bool
waitNamedPipe name timeout =
    withCString name $ \ c_name -> do
      r <- c_WaitNamedPipe c_name timeout
      e <- getLastError
      if | r                      -> pure r
         | e == eRROR_SEM_TIMEOUT -> pure False
         | otherwise              -> failWith "waitNamedPipe" e


-- 'c_WaitNamedPipe' is a blocking call, hence the _safe_ import.
foreign import ccall safe "windows.h WaitNamedPipeA"
  c_WaitNamedPipe :: LPCSTR -- lpNamedPipeName
                  -> DWORD  -- nTimeOut
                  -> IO BOOL

eRROR_SEM_TIMEOUT :: ErrCode
eRROR_SEM_TIMEOUT = #const ERROR_SEM_TIMEOUT
