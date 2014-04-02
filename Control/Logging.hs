{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Control.Logging
    ( log
    , warn
    , debug
    , errorL
    , traceL
    , traceShowL
    , withLogging
    , flushLog
    , setDebugLevel
    ) where

import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.IORef
import Data.Monoid
import Data.Text as T
import Data.Thyme
import Debug.Trace
import System.IO.Unsafe
import System.Locale
import System.Log.FastLogger
import Prelude hiding (log)

logLevel :: IORef LogLevel
{-# NOINLINE logLevel #-}
logLevel = unsafePerformIO $ newIORef LevelDebug

setDebugLevel :: LogLevel -> IO ()
setDebugLevel = atomicWriteIORef logLevel

logSet :: IORef LoggerSet
{-# NOINLINE logSet #-}
logSet = unsafePerformIO $ do
    set <- newStdoutLoggerSet defaultBufSize
    newIORef set

logger :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> IO ()
logger _loc !src !lvl str = do
    maxLvl <- readIORef logLevel
    when (lvl >= maxLvl) $ do
        now <- getCurrentTime
        let stamp = formatTime defaultTimeLocale "%b-%d %H:%M:%S%Q" now
        set <- readIORef logSet
        pushLogStr set
            $ toLogStr (stamp ++ " " ++ renderLevel lvl
                              ++ " " ++ renderSource src)
            <> toLogStr str
  where
    renderSource :: Text -> String
    renderSource txt
        | T.null txt = ""
        | otherwise  = unpack txt ++ ": "

    renderLevel LevelDebug = "[DEBUG]"
    renderLevel LevelInfo  = "[INFO]"
    renderLevel LevelWarn  = "[WARN]"
    renderLevel LevelError = "[ERROR]"
    renderLevel (LevelOther txt) = "[" ++ unpack txt ++ "]"

withLogging :: (MonadBaseControl IO m, MonadIO m) => m a -> m a
withLogging = (`finally` flushLog)

flushLog :: MonadIO m => m ()
flushLog = liftIO $ do
    set <- readIORef logSet
    flushLogStr set

instance MonadLogger IO where
    monadLoggerLog = logger

log :: MonadLogger m => Text -> m ()
log = logInfoN

debug :: MonadLogger m => Text -> m ()
debug = logDebugN

warn :: MonadLogger m => Text -> m ()
warn = logWarnN

errorL :: Text -> a
errorL str = error (unsafePerformIO (logErrorN str) `seq` unpack str)

traceL :: Text -> a -> a
traceL str = trace (unsafePerformIO (logDebugN str) `seq` unpack str)

traceShowL :: Show a => a -> a1 -> a1
traceShowL x =
    let s = show x
    in trace (unsafePerformIO (logDebugN (pack s)) `seq` s)
