`logging` is a wrapper around `monad-logger` and `fast-logger` which makes it
easy to log from any `MonadLogger` environment, or from `IO`.  It provides the
following conveniences on top of those libraries:

  - A MonadLogger instance for IO.  Usually this is bad for libraries, but can
    be very convenience for application writers who know they always want to
    log from IO to the console.  If you need to log to other sources, or to
    make logging compile-time optional, use `monad-logger` directly.

  - A set of shorter functions to type: `debug`, `log`, `warn`.

  - Logging variants of `error`, `trace` and `traceShow`, called `errorL`,
    `traceL` and `traceShowL`.  These use 'unsafePerformIO' in order to act as
    direct replacements, so the usual caveats apply.

  - STM variants: `debugS`, `logS`, `warnS`, `errorS`, `traceS` and
    `traceShowS`.  These log to a tracing buffer until the transaction is
    completed, at which point they are logged in IO.  This means that logging
    statements will be delayed, and may well be duplicated if the transaction
    has retried.

  - A global function, `setDebugLevel`, which uses a global `IORef` to record
    the logging level, saving you from having to carry around the notion of
    "verbosity level" in a Reader environment.

  - A set of "timed" variants, 'timedDebug', 'timedLog', 'timedWarn', which
    report how long the specified action took to execute in wall-clock time.
