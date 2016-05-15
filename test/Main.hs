module Main where

import Control.Concurrent
import Control.Exception
import Control.Logging
import Prelude hiding (log)
import Test.Hspec

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

main :: IO ()
main = hspec $

    describe "simple logging" $ do

        it "logs output" $ withStdoutLogging $ do
            log "Hello, world!"
            warnS "test-suite" "you've been warned"
            timedLog "Did a good thing" $ threadDelay 100000
            _ <- tryAny $ timedLog "Did a bad thing" $
                threadDelay 100000 >> error "foo"
            _ <- tryAny $ errorL "Uh oh"
            return ()

        it "supports setting log levels" $ do
             setLogLevel LevelWarn

             withStdoutLogging $ do
                 debugS "Set LogLevel test" "This is an unshown debug message"
                 logS "Set LogLevel test" "This is an unshown info message"
                 warnS "Set LogLevel test" "This is a shown info message"
                 _ <- tryAny $ errorSL "Set LogLevel test" "This is a shown error message"
                 return ()

             -- setting the log level back to debug so that following tests run
             setLogLevel LevelDebug

        it "supports using debug classes" $ do
            setDebugSourceRegex "(foo\\.|baaz\\.).*"
            withStdoutLogging $ do
                debugS "foo" "This is an unshown debug message"
                debugS "foo.bar" "This is a shown debug message"
                debugS "baaz.quux" "This is a shown debug message"
                -- checking that non-debug messages aren't filtered
                logS "bar" "This is a shown log message"
                logS "foo.bar" "This is a shown log message"
                warnS "foo" "This is a shown warn message"
