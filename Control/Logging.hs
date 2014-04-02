{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Control.Logging
    ( log
    , warn
    , debug
    , errorL
    , traceL
    , traceShowL
    , timedLog
    , timedLog'
    , timedDebug
    , timedDebug'
    , withStdoutLogging
    , withStderrLogging
    , flushLog
    , setDebugLevel
    , setLogFormat
    ) where

import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.AffineSpace
import Data.IORef
import Data.Monoid
import Data.Text as T
import Data.Thyme
import Debug.Trace
import Prelude hiding (log)
import System.IO.Unsafe
import System.Locale
import System.Log.FastLogger

logLevel :: IORef LogLevel
{-# NOINLINE logLevel #-}
logLevel = unsafePerformIO $ newIORef LevelDebug

-- | Set the verbosity level.  Messages at our higher than this level are
--   displayed.  It defaults to 'LevelDebug'.
setDebugLevel :: LogLevel -> IO ()
setDebugLevel = atomicWriteIORef logLevel

logSet :: IORef LoggerSet
{-# NOINLINE logSet #-}
logSet = unsafePerformIO $
    newIORef (error "Must call withStdoutLogging or withStderrLogging")

logFormat :: IORef String
{-# NOINLINE logFormat #-}
logFormat = unsafePerformIO $ newIORef "%Y %b-%d %H:%M:%S%Q"

-- | Set the format used for log timestamps.
setLogFormat :: String -> IO ()
setLogFormat = atomicWriteIORef logFormat

logger :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> IO ()
logger _loc !src !lvl str = do
    maxLvl <- readIORef logLevel
    when (lvl >= maxLvl) $ do
        now <- getCurrentTime
        fmt <- readIORef logFormat
        let stamp = formatTime defaultTimeLocale fmt now
        set <- readIORef logSet
        pushLogStr set
            $ toLogStr (stamp ++ " " ++ renderLevel lvl
                              ++ " " ++ renderSource src)
            <> toLogStr str
            <> toLogStr (pack "\n")
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

-- | This function, or 'withStderrLogging', must be wrapped around whatever
--   region of your application intends to use logging.  Typically it would be
--   wrapped around the body of 'main'.
withStdoutLogging :: (MonadBaseControl IO m, MonadIO m) => m a -> m a
withStdoutLogging f = do
    liftIO $ do
        set <- newStdoutLoggerSet defaultBufSize
        atomicWriteIORef logSet set
    f `finally` flushLog

withStderrLogging :: (MonadBaseControl IO m, MonadIO m) => m a -> m a
withStderrLogging f = do
    liftIO $ do
        set <- newStderrLoggerSet defaultBufSize
        atomicWriteIORef logSet set
    f `finally` flushLog

flushLog :: MonadIO m => m ()
flushLog = liftIO $ do
    set <- readIORef logSet
    flushLogStr set

instance MonadLogger IO where
    monadLoggerLog = logger

-- | Synonym for 'Control.Monad.Logger.logInfoN'.  This module provides a
--   'MonadLogger' instance for IO, so this functions can be used directly in
--   IO.  The only requirement is that you must surround the body of your
--   @main@ function with a call to 'withStdoutLogging' or
--   'withStderrLogging', to ensure that all logging buffers are flushed.
log :: MonadLogger m => Text -> m ()
log = logInfoN

debug :: MonadLogger m => Text -> m ()
debug = logDebugN

warn :: MonadLogger m => Text -> m ()
warn = logWarnN

-- | A logging variant of 'error' which uses 'unsafePerformIO' to output a log
--   message before calling 'error'.
errorL :: Text -> a
errorL str = error (unsafePerformIO (logErrorN str) `seq` unpack str)

traceL :: Text -> a -> a
traceL str = trace (unsafePerformIO (logDebugN str) `seq` unpack str)

traceShowL :: Show a => a -> a1 -> a1
traceShowL x =
    let s = show x
    in trace (unsafePerformIO (logDebugN (pack s)) `seq` s)

doTimedLog :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
         => (Text -> m ()) -> Bool -> Text -> m () -> m ()
doTimedLog logf wrapped msg f = do
    start <- liftIO getCurrentTime
    when wrapped $ logf $ msg <> "..."
    f `catch` \e -> do
        let str = show (e :: SomeException)
        wrapup start $ pack $
            if wrapped
            then "...FAIL (" ++ str ++ ")"
            else " (FAIL: " ++ str ++ ")"
        throwIO e
    wrapup start $ if wrapped then "...done" else ""
  where
    wrapup start m = do
        end <- liftIO getCurrentTime
        logf $ msg <> m <> " [" <> pack (show (end .-. start)) <> "]"

-- | Output a logging message both before an action begins, and after it ends,
--   reporting the total length of time.  If an exception occurred, it is also
--   reported.
timedLog :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
         => Text -> m () -> m ()
timedLog = doTimedLog log True

-- | Like 'timedLog', except that it does only logs when the action has
--   completed or faileda.
timedLog' :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
          => Text -> m () -> m ()
timedLog' = doTimedLog log False

-- | A debug variant of 'timedLog'.
timedDebug :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
           => Text -> m () -> m ()
timedDebug = doTimedLog debug True

timedDebug' :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
            => Text -> m () -> m ()
timedDebug' = doTimedLog debug False
