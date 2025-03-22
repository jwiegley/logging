{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Quick example of how to use this module:
--
-- @
-- import Control.Logging
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
    , LogLevel (..)
    ) where

import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Functor ((<$))
import Data.IORef
import Data.Maybe (isJust)
import Data.Monoid
import Data.Text as T
import Data.Time
import Data.Time.Locale.Compat (defaultTimeLocale)
import Prelude hiding (log)
import System.IO.Unsafe
import System.Log.FastLogger
import Text.Regex (Regex, mkRegex, matchRegex)

data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther Text
    deriving (Eq, Prelude.Show, Prelude.Read, Ord)

type LogSource = Text

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
logTimeFormat = unsafePerformIO $ newIORef "%F %T"

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
        . mkRegex


loggingLogger :: ToLogStr msg => LogLevel -> LogSource -> msg -> IO ()
loggingLogger !lvl !src str = do
    maxLvl <- readIORef logLevel
    when (lvl >= maxLvl) $ do
        mre <- readIORef debugSourceRegexp
        let willLog = case mre of
                Nothing -> True
                Just re -> lvl /= LevelDebug || isJust (matchRegex re (T.unpack src))
        when willLog $ do
            now <- getZonedTime
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

-- You must surround the body of your @main@ function with a call to
-- 'withStdoutLogging' or 'withStderrLogging', to ensure that all logging
-- buffers are properly flushed on exit.
log :: Text -> IO ()
log = loggingLogger LevelInfo ""

logError :: Text -> Text -> IO ()
logError = loggingLogger LevelError

logS :: Text -> Text -> IO ()
logS = loggingLogger LevelInfo

-- | The apostrophe varients of the logging functions flush the log after each
--   message.
log' :: MonadIO m => Text -> m ()
log' msg = liftIO (log msg) >> flushLog

logS' :: MonadIO m => Text -> Text -> m ()
logS' src msg = liftIO (logS src msg) >> flushLog

debug :: Text -> IO ()
debug = debugS ""

debugS :: Text -> Text -> IO ()
debugS = loggingLogger LevelDebug

debug' :: MonadIO m => Text -> m ()
debug' msg = liftIO (debug msg) >> flushLog

debugS' :: MonadIO m => Text -> Text -> m ()
debugS' src msg = liftIO (debugS src msg) >> flushLog

warn :: Text -> IO ()
warn = warnS ""

warnS :: Text -> Text -> IO ()
warnS = loggingLogger LevelWarn

warn' :: MonadIO m => Text -> m ()
warn' msg = liftIO (warn msg) >> flushLog

warnS' :: MonadIO m => Text -> Text -> m ()
warnS' src msg = liftIO (warnS src msg) >> flushLog

-- | A logging variant of 'error' which uses 'unsafePerformIO' to output a log
--   message before calling 'error'.
errorL :: Text -> a
errorL str = error (unsafePerformIO (logError "" str) `seq` unpack str)

errorL' :: Text -> a
errorL' str = error (unsafePerformIO (logError "" str >> flushLog) `seq` unpack str)

errorSL :: Text -> Text -> a
errorSL src str = error (unsafePerformIO (logError src str) `seq` unpack str)

errorSL' :: Text -> Text -> a
errorSL' src str =
    error (unsafePerformIO (logError src str >> flushLog) `seq` unpack str)

traceL :: Text -> a -> a
traceL str x = unsafePerformIO (debug str) `seq` x

traceL' :: Text -> a -> a
traceL' str x = unsafePerformIO (debug str >> flushLog) `seq` x

traceSL :: Text -> Text -> a -> a
traceSL src str x = unsafePerformIO (debugS src str) `seq` x

traceSL' :: Text -> Text -> a -> a
traceSL' src str x =
    unsafePerformIO (debugS src str >> flushLog) `seq` x

traceShowL :: Show a => a -> a
traceShowL x =
    let s = Prelude.show x
    in unsafePerformIO (debug (pack s)) `seq` x

traceShowL' :: Show a => a -> a
traceShowL' x =
    let s = Prelude.show x
    in unsafePerformIO (debug (pack s) >> flushLog) `seq` x

traceShowSL :: Show a => Text -> a -> a
traceShowSL src x =
    let s = Prelude.show x
    in unsafePerformIO (debugS src (pack s)) `seq` x

traceShowSL' :: Show a => Text -> a -> a
traceShowSL' src x =
    let s = Prelude.show x
    in unsafePerformIO (debugS src (pack s) >> flushLog) `seq` x

doTimedLog :: (MonadBaseControl IO m, MonadIO m)
           => (Text -> IO ()) -> Bool -> Text -> m a -> m a
doTimedLog logf wrapped msg f = do
    start <- liftIO getCurrentTime
    when wrapped $ (liftIO . logf) $ msg <> "..."
    res <- f `catch` \e -> do
        let str = Prelude.show (e :: SomeException)
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
        liftIO . logf $
          msg <> m <> " [" <> pack (Prelude.show (diffUTCTime end start)) <> "]"

-- | Output a logging message both before an action begins, and after it ends,
--   reporting the total length of time.  If an exception occurred, it is also
--   reported.
timedLog :: (MonadBaseControl IO m, MonadIO m)
         => Text -> m a -> m a
timedLog = doTimedLog log True

timedLog' :: (MonadBaseControl IO m, MonadIO m)
          => Text -> m a -> m a
timedLog' msg f = doTimedLog log True msg f >>= (<$ flushLog)

timedLogS :: (MonadBaseControl IO m, MonadIO m)
          => Text -> Text -> m a -> m a
timedLogS src = doTimedLog (logS src) True

timedLogS' :: (MonadBaseControl IO m, MonadIO m)
           => Text -> Text -> m a -> m a
timedLogS' src msg f = doTimedLog (logS src) True msg f >>= (<$ flushLog)

-- | Like 'timedLog', except that it does only logs when the action has
--   completed or failed after it is done.
timedLogEnd :: (MonadBaseControl IO m, MonadIO m)
          => Text -> m a -> m a
timedLogEnd = doTimedLog log False

timedLogEnd' :: (MonadBaseControl IO m, MonadIO m)
             => Text -> m a -> m a
timedLogEnd' msg f = doTimedLog log False msg f >>= (<$ flushLog)

timedLogEndS :: (MonadBaseControl IO m, MonadIO m)
             => Text -> Text -> m a -> m a
timedLogEndS src = doTimedLog (logS src) False

timedLogEndS' :: (MonadBaseControl IO m, MonadIO m)
              => Text -> Text -> m a -> m a
timedLogEndS' src msg f = doTimedLog (logS src) False msg f >>= (<$ flushLog)

-- | A debug variant of 'timedLog'.
timedDebug :: (MonadBaseControl IO m, MonadIO m)
           => Text -> m a -> m a
timedDebug = doTimedLog debug True

timedDebug' :: (MonadBaseControl IO m, MonadIO m)
             => Text -> m a -> m a
timedDebug' msg f = doTimedLog debug True msg f >>= (<$ flushLog)

timedDebugS :: (MonadBaseControl IO m, MonadIO m)
            => Text -> Text -> m a -> m a
timedDebugS src = doTimedLog (debugS src) True

timedDebugS' :: (MonadBaseControl IO m, MonadIO m)
             => Text -> Text -> m a -> m a
timedDebugS' src msg f = doTimedLog (debugS src) True msg f >>= (<$ flushLog)

timedDebugEnd :: (MonadBaseControl IO m, MonadIO m)
              => Text -> m a -> m a
timedDebugEnd = doTimedLog debug False

timedDebugEnd' :: (MonadBaseControl IO m, MonadIO m)
               => Text -> m a -> m a
timedDebugEnd' msg f = doTimedLog debug False msg f >>= (<$ flushLog)

timedDebugEndS :: (MonadBaseControl IO m, MonadIO m)
               => Text -> Text -> m a -> m a
timedDebugEndS src = doTimedLog (debugS src) False

timedDebugEndS' :: (MonadBaseControl IO m, MonadIO m)
                => Text -> Text -> m a -> m a
timedDebugEndS' src msg f = doTimedLog (debugS src) False msg f >>= (<$ flushLog)
