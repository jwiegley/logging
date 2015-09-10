# logging

`logging` is a wrapper around `fast-logger` which makes
it easy to log from `IO`. It provides the following conveniences on top of
those libraries:

- A set of shorter functions to type: `debug`, `log`, `warn`, plus others
  that flush after each message, or which allow providing a message source
  string.

- Logging variants of `error`, `trace` and `traceShow`, called `errorL`,
  `traceL` and `traceShowL`.  These use `unsafePerformIO` in order to act as
  direct replacements, so the usual caveats apply.

- A global function, `setDebugLevel`, which uses a global `IORef` to record
  the logging level, saving you from having to carry around the notion of
  "verbosity level" in a Reader environment.

- A set of "timed" variants, `timedLog` and `timedDebug`, which report how
  long the specified action took to execute in wall-clock time.
