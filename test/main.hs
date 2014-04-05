module Main where

import Control.Concurrent
import Control.Exception
import Control.Logging
import Control.Monad.Logger
import Prelude hiding (log)
import Test.Hspec

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

main :: IO ()
main = hspec $ do
    describe "simple logging" $ do
        it "logs output" $ (withStdoutLogging :: IO () -> IO ())  $ do
            log "Hello, world!"
            timedLog "Did a good thing" $ threadDelay 100000
            _ <- tryAny $ timedLog "Did a bad thing" $
                threadDelay 100000 >> error "foo"
            _ <- tryAny $ errorL "Uh oh"
            return ()

        it "can be passed to runLoggingT" $ do
            flip runLoggingT loggingLogger (log "Hello" :: LoggingT IO ())
            flushLog

        it "supports using debug classes" $ do
            setDebugSourceRegex "foo\\..*"
            withStdoutLogging $ do
                debugS "foo" "This is a foo message"
                debugS "foo.bar" "This is a foo.bar message"
