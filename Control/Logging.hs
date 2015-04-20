{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

-- | Quick example of how to use this module:
--
-- @import Control.Logging
--
-- main = withStdoutLogging $ do
--     log "This is a log message!"
--     timedLog "This is a timed log message!" $ threadDelay 100000
-- @

module Control.Logging
    ( log
    , log'
    , logS
    , logS'
    , warn
    , warn'
    , warnS
    , warnS'
    , debug
    , debug'
    , debugS
    , debugS'
    , errorL
    , errorL'
    , errorSL
    , errorSL'
    , traceL
    , traceL'
    , traceSL
    , traceSL'
    , traceShowL
    , traceShowL'
    , traceShowSL
    , traceShowSL'
    , timedLog
    , timedLog'
    , timedLogS
    , timedLogS'
    , timedLogEnd
    , timedLogEnd'
    , timedLogEndS
    , timedLogEndS'
    , timedDebug
    , timedDebug'
    , timedDebugS
    , timedDebugS'
    , timedDebugEnd
    , timedDebugEnd'
    , timedDebugEndS
    , timedDebugEndS'
    , withStdoutLogging
    , withStderrLogging
    , withFileLogging
    , flushLog
    , loggingLogger
    , setLogLevel
    , setLogTimeFormat
    , setDebugSourceRegex
    ) where

import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Functor ((<$))
import Data.IORef
import Data.Maybe (isJust)
import Data.Monoid
import Data.Text as T
import Data.Text.Encoding as T
import Data.Time
import Debug.Trace
import Prelude hiding (log)
import System.IO.Unsafe
#if MIN_VERSION_time (1,5,0)
import System.Locale hiding (defaultTimeLocale)
#else
import System.Locale
#endif
import System.Log.FastLogger
import Text.Regex.PCRE.Light

logLevel :: IORef LogLevel
{-# NOINLINE logLevel #-}
logLevel = unsafePerformIO $ newIORef LevelDebug

-- | Set the verbosity level.  Messages at our higher than this level are
--   displayed.  It defaults to 'LevelDebug'.
setLogLevel :: LogLevel -> IO ()
setLogLevel = atomicWriteIORef logLevel

logSet :: IORef LoggerSet
{-# NOINLINE logSet #-}
logSet = unsafePerformIO $
    newIORef (error "Must call withStdoutLogging or withStderrLogging")

logTimeFormat :: IORef String
{-# NOINLINE logTimeFormat #-}
logTimeFormat = unsafePerformIO $ newIORef "%Y %b-%d %H:%M:%S%Q"

-- | Set the format used for log timestamps.
setLogTimeFormat :: String -> IO ()
setLogTimeFormat = atomicWriteIORef logTimeFormat

debugSourceRegexp :: IORef (Maybe Regex)
{-# NOINLINE debugSourceRegexp #-}
debugSourceRegexp = unsafePerformIO $ newIORef Nothing

-- | When printing 'LevelDebug' messages, only display those matching the
--   given regexp applied to the Source parameter.  Calls to 'debug' without a
--   source parameter are regarded as having a source of @""@.
setDebugSourceRegex :: String -> IO ()
setDebugSourceRegex =
    atomicWriteIORef debugSourceRegexp
        . Just
        . flip compile []
        . encodeUtf8
        . T.pack

-- | This function is used to implement 'monadLoggerLog' for the IO instance.
--   You may reuse it if you wish, or it can be passed as an argument to
--   'runLoggingT' -- in which case you must remember to call 'flushLog'
--   before the program exits.
loggingLogger :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> IO ()
loggingLogger _loc !src !lvl str = do
    maxLvl <- readIORef logLevel
    when (lvl >= maxLvl) $ do
        mre <- readIORef debugSourceRegexp
        let willLog = case mre of
                Nothing -> True
                Just re -> isJust (match re (encodeUtf8 src) [])
        when willLog $ do
            now <- getCurrentTime
            fmt <- readIORef logTimeFormat
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

withFileLogging :: (MonadBaseControl IO m, MonadIO m) => FilePath -> m a -> m a
withFileLogging path f = do
    liftIO $ do
        set <- newFileLoggerSet defaultBufSize path
        atomicWriteIORef logSet set
    f `finally` flushLog

-- | Flush all collected logging messages.  This is automatically called by
--   'withStdoutLogging' and 'withStderrLogging' when those blocks are exited
--   by whatever means.
flushLog :: MonadIO m => m ()
flushLog = liftIO $ do
    set <- readIORef logSet
    flushLogStr set

instance MonadLogger IO where
    monadLoggerLog = loggingLogger

-- | Synonym for 'Control.Monad.Logger.logInfoN'.  This module provides a
--   'MonadLogger' instance for IO, so this function can be used directly in
--   IO.  The only requirement is that you must surround the body of your
--   @main@ function with a call to 'withStdoutLogging' or
--   'withStderrLogging', to ensure that all logging buffers are properly
--   flushed on exit.
log :: MonadLogger m => Text -> m ()
log = logInfoN

logS :: MonadLogger m => Text -> Text -> m ()
logS = logInfoNS

-- | The apostrophe varients of the logging functions flush the log after each
--   message.
log' :: (MonadLogger m, MonadIO m) => Text -> m ()
log' msg = log msg >> flushLog

logS' :: (MonadLogger m, MonadIO m) => Text -> Text -> m ()
logS' src msg = logS src msg >> flushLog

debug :: MonadLogger m => Text -> m ()
debug = logDebugN

debugS :: MonadLogger m => Text -> Text -> m ()
debugS = logDebugNS

debug' :: (MonadLogger m, MonadIO m) => Text -> m ()
debug' msg = debug msg >> flushLog

debugS' :: (MonadLogger m, MonadIO m) => Text -> Text -> m ()
debugS' src msg = debugS src msg >> flushLog

warn :: MonadLogger m => Text -> m ()
warn = logWarnN

warnS :: MonadLogger m => Text -> Text -> m ()
warnS = logWarnNS

warn' :: (MonadLogger m, MonadIO m) => Text -> m ()
warn' msg = warn msg >> flushLog

warnS' :: (MonadLogger m, MonadIO m) => Text -> Text -> m ()
warnS' src msg = warnS src msg >> flushLog

-- | A logging variant of 'error' which uses 'unsafePerformIO' to output a log
--   message before calling 'error'.
errorL :: Text -> a
errorL str = error (unsafePerformIO (logErrorN str) `seq` unpack str)

errorL' :: Text -> a
errorL' str = error (unsafePerformIO (logErrorN str >> flushLog) `seq` unpack str)

errorSL :: Text -> Text -> a
errorSL src str = error (unsafePerformIO (logErrorNS src str) `seq` unpack str)

errorSL' :: Text -> Text -> a
errorSL' src str =
    error (unsafePerformIO (logErrorNS src str >> flushLog) `seq` unpack str)

traceL :: Text -> a -> a
traceL str = trace (unsafePerformIO (logDebugN str) `seq` unpack str)

traceL' :: Text -> a -> a
traceL' str = trace (unsafePerformIO (logDebugN str >> flushLog) `seq` unpack str)

traceSL :: Text -> Text -> a -> a
traceSL src str = trace (unsafePerformIO (logDebugNS src str) `seq` unpack str)

traceSL' :: Text -> Text -> a -> a
traceSL' src str =
    trace (unsafePerformIO (logDebugNS src str >> flushLog) `seq` unpack str)

traceShowL :: Show a => a -> a1 -> a1
traceShowL x =
    let s = show x
    in trace (unsafePerformIO (logDebugN (pack s)) `seq` s)

traceShowL' :: Show a => a -> a1 -> a1
traceShowL' x =
    let s = show x
    in trace (unsafePerformIO (logDebugN (pack s) >> flushLog) `seq` s)

traceShowSL :: Show a => Text -> a -> a1 -> a1
traceShowSL src x =
    let s = show x
    in trace (unsafePerformIO (logDebugNS src (pack s)) `seq` s)

traceShowSL' :: Show a => Text -> a -> a1 -> a1
traceShowSL' src x =
    let s = show x
    in trace (unsafePerformIO (logDebugNS src (pack s) >> flushLog) `seq` s)

doTimedLog :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
         => (Text -> m ()) -> Bool -> Text -> m a -> m a
doTimedLog logf wrapped msg f = do
    start <- liftIO getCurrentTime
    when wrapped $ logf $ msg <> "..."
    res <- f `catch` \e -> do
        let str = show (e :: SomeException)
        wrapup start $ pack $
            if wrapped
            then "...FAIL (" ++ str ++ ")"
            else " (FAIL: " ++ str ++ ")"
        throwIO e
    wrapup start $ if wrapped then "...done" else ""
    return res
  where
    wrapup start m = do
        end <- liftIO getCurrentTime
        logf $ msg <> m <> " [" <> pack (show (diffUTCTime end start)) <> "]"

-- | Output a logging message both before an action begins, and after it ends,
--   reporting the total length of time.  If an exception occurred, it is also
--   reported.
timedLog :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
         => Text -> m a -> m a
timedLog = doTimedLog log True

timedLog' :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
          => Text -> m a -> m a
timedLog' msg f = doTimedLog log True msg f >>= (<$ flushLog)

timedLogS :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
          => Text -> Text -> m a -> m a
timedLogS src = doTimedLog (logS src) True

timedLogS' :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
           => Text -> Text -> m a -> m a
timedLogS' src msg f = doTimedLog (logS src) True msg f >>= (<$ flushLog)

-- | Like 'timedLog', except that it does only logs when the action has
--   completed or failed after it is done.
timedLogEnd :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
          => Text -> m a -> m a
timedLogEnd = doTimedLog log False

timedLogEnd' :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
             => Text -> m a -> m a
timedLogEnd' msg f = doTimedLog log False msg f >>= (<$ flushLog)

timedLogEndS :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
             => Text -> Text -> m a -> m a
timedLogEndS src = doTimedLog (logS src) False

timedLogEndS' :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
              => Text -> Text -> m a -> m a
timedLogEndS' src msg f = doTimedLog (logS src) False msg f >>= (<$ flushLog)

-- | A debug variant of 'timedLog'.
timedDebug :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
           => Text -> m a -> m a
timedDebug = doTimedLog debug True

timedDebug' :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
             => Text -> m a -> m a
timedDebug' msg f = doTimedLog debug True msg f >>= (<$ flushLog)

timedDebugS :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
            => Text -> Text -> m a -> m a
timedDebugS src = doTimedLog (debugS src) True

timedDebugS' :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
             => Text -> Text -> m a -> m a
timedDebugS' src msg f = doTimedLog (debugS src) True msg f >>= (<$ flushLog)

timedDebugEnd :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
              => Text -> m a -> m a
timedDebugEnd = doTimedLog debug False

timedDebugEnd' :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
               => Text -> m a -> m a
timedDebugEnd' msg f = doTimedLog debug False msg f >>= (<$ flushLog)

timedDebugEndS :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
               => Text -> Text -> m a -> m a
timedDebugEndS src = doTimedLog (debugS src) False

timedDebugEndS' :: (MonadLogger m, MonadBaseControl IO m, MonadIO m)
                => Text -> Text -> m a -> m a
timedDebugEndS' src msg f = doTimedLog (debugS src) False msg f >>= (<$ flushLog)
