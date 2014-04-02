module Main where

-- import Control.Concurrent
import Control.Logging
import Test.Hspec
import Prelude hiding (log)

main :: IO ()
main = hspec $ do
    describe "simple logging" $
        it "logs output" $ (withLogging :: IO () -> IO ())  $
            log "Hello, world!"
