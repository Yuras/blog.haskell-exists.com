---
title: Handling (async) exceptions in haskell: snap-server (case study)
published: 2014-11-22
---
(Originally posted [here](https://github.com/Yuras/io-region/wiki/Handling-%28async%29-exceptions-in-haskell:-snap-server-%28case-study%29))

Exception handling is hard, and asynchronous exceptions make it even harder.
But there are common patterns, that simplifies exception handling and make our
life much easier. Here we will explore a widely used open source library,
`snap-server`, and identify common issues, difficulties and mistakes. Also we'll
see how to avoid most of the mistakes and describe useful patterns. Thanks to
Gregory Collins for letting me use snap-server here.

(I probably should note, that I'm snapframework user, and I like it. So the
criticism here is friendly in it's nature.)

The post is organized into a series of examples of real code. I'll use
[this](https://github.com/snapframework/snap-server/tree/a539ac3dc7eafdff5e61b29591bc81fd9976b529)
specific source code tree. We'll mostly discuss simple, obvious cases.


## example #1

```haskell
sendFileFunc :: Socket -> SendFileHandler
sendFileFunc sock !_ builder fPath offset nbytes = bracket acquire closeFd go
  where
    sockFd    = Fd (fdSocket sock)
    acquire   = openFd fPath ReadOnly Nothing defaultFileFlags
    go fileFd = do sendHeaders builder sockFd
                   sendFile sockFd fileFd offset nbytes
```

[Source](https://github.com/snapframework/snap-server/blob/a539ac3dc7eafdff5e61b29591bc81fd9976b529/src/Snap/Internal/Http/Server/Socket.hs#L113)

That is an example how your exception handling code should look like. Here
`bracket` does most of heavy lifting to prepare safe environment. Acquire and
cleanup actions effectively are library functions (from `unix` package).


### Contract

So the function on itself is perfect. But it relies on library functions to be
correct. Lets formulate explicitly the contract we are expecting:

* `openFd` should not leak file descriptor if it fails.

That is pretty natural requirement, because we will not get the file
descriptor at all if `openFd` fails, so there is nothing we can do on our
side.

* `closeFd` should not leak file descriptor if it fails.

That doesn't look so natural, but the reason is the same -- there is nothing
we can do in case of failure.

Note: the contract should be preserved even in case of async exception,
because there is no good way to distinguish async and sync exception.

In the ideal world we should not simply assume the contract is preserved, we
should read documentation instead. Unfortunately the is no explicit contract
in the documentation. Probably we should pester the maintainer. Anyway, we can
either look for other library or use the existing.


### Inspecting source code of dependencies

One may think that we should inspect source code of `openFd` and `closeFd`. It
may be useful, but it doesn't solve the issue. Unless the contract is
explicitly states, the author is free to change it. But you can inspect the
particular version of the `unix` package and commit yourself to it.


## example #2

```haskell
withLoggers afp efp act =
    bracket (do mvar <- newMVar ()
                let f s = withMVar mvar
                            (const $ S.hPutStr stderr s >> hFlush stderr)
                alog <- maybeSpawnLogger f afp
                elog <- maybeSpawnLogger f efp
                return (alog, elog))
            (\(alog, elog) -> do
                maybe (return ()) stopLogger alog
                maybe (return ()) stopLogger elog)
            (\(alog, elog) -> act ( liftM logMsg alog <|> maybeIoLog afp
                                  , liftM logMsg elog <|> maybeIoLog efp))
```

[Source](https://github.com/snapframework/snap-server/blob/a539ac3dc7eafdff5e61b29591bc81fd9976b529/src/Snap/Http/Server.hs#L183)

The function spawns two loggers (using `maybeSpawnLogger`) and stops them
(using `stopLogger`) on exit. It is not important what loggers do, but you
can check the source code if you are interested.

Do you see any issue here? Probably not. Lets say the function has no obvious
issues. (Actually I found one minor issue, but I spent half an hour reading
code.) But it doesn't mean the function is correct.


### Don't assume anything

I convinced myself that `maybeSpawnLogger` and `stopLogger` never throw
exception. (Well, `stopLogger` is interruptible, so it actually can
sometimes.) But is there any reason we need to assume that? What if someone
change them when refactoring? Sometimes we need to rely on actions not to
throw exception, but not here. Lets fix it:

```haskell
withLoggers afp efp act =
    bracket (do mvar <- newMVar ()
                let f s = withMVar mvar
                            (const $ S.hPutStr stderr s >> hFlush stderr)
                bracketOnError
                  (maybeSpawnLogger f afp)
                  (maybe (return ()) stopLogger)
                  (\alog -> do
                    elog <- maybeSpawnLogger f efp
                    return (alog, elog))

            (\(alog, elog) -> do
                maybe (return ()) stopLogger alog
                `finally`
                maybe (return ()) stopLogger elog)
            (\(alog, elog) -> act ( liftM logMsg alog <|> maybeIoLog afp
                                  , liftM logMsg elog <|> maybeIoLog efp))
```

Here we did two fixes. First, we use `bracketOnError` to ensure the first
logger will be stopped if we fail to spawn the second. Also `finally` is used
to ensure the second logger will be stopped if the first one fails to stop.
Pretty complicated, but now it is at least correct.

Note that the code is correct w.r.t. both sync and async exception. Here we
did nothing special to handle async exceptions.

However the function still relies on `maybeSpawnLogger` and `stopLogger` to
preserve the contract (see example #1).


### Keep it simple

The function is still far from ideal. In the acquire action it does
unnecessary work -- allocates `MVar`. Is there any reason to do that inside
`bracket`? Probably not. Lets simplify the code to make reasoning easer:


```haskell
withLoggers afp efp act = do
    mvar <- newMVar ()
    let f s = withMVar mvar
                (const $ S.hPutStr stderr s >> hFlush stderr)

    bracket (do bracketOnError
                  (maybeSpawnLogger f afp)
                  (maybe (return ()) stopLogger)
                  (\alog -> do
                    elog <- maybeSpawnLogger f efp
                    return (alog, elog))

            (\(alog, elog) -> do
                maybe (return ()) stopLogger alog
                `finally`
                maybe (return ()) stopLogger elog)
            (\(alog, elog) -> act ( liftM logMsg alog <|> maybeIoLog afp
                                  , liftM logMsg elog <|> maybeIoLog efp))
```


### Divide and conquer

The function is very complex and hard to reason still. But notice that there
is no dependency between loggers here, so we can handle them separately. Lets
use two brackets, one per logger:

```haskell
withLoggers afp efp act = do
    mvar <- newMVar ()
    let f s = withMVar mvar
                (const $ S.hPutStr stderr s >> hFlush stderr)

    bracket (maybeSpawnLogger f afp)
            (maybe (return ()) stopLogger) $ \alog -> do

      bracket (maybeSpawnLogger f efp)
              (maybe (return ()) stopLogger) $ \elog -> do

        act ( liftM logMsg alog <|> maybeIoLog afp
            , liftM logMsg elog <|> maybeIoLog efp))
```

Now the function consists of two nested handlers, that are very similar to
example #1. It is obviously correct.


### Local reasoning

I'd like to draw your attention to the next. We reason about exception
handling locally. Obviously all functions we use here affect exception safety
of our code, but we use the contract (see example #1) to build a wall between
our code and it's dependencies. That is the only way to handle exceptions.
Sometimes the contract is more complex, and we have to provide safe
environment for our dependencies, but that should be explicitly stated in
documentation.

We'll discuss `maybeSpawnLogger` and `stopLogger` later, and you'll see that
the contract protects us from caller site too, allowing local reasoning.


## example #3

Lets inspect `maybeSpawnLogger`. It is a simple wrapper over other function:

```haskell
maybeSpawnLogger f (ConfigFileLog fp) =
    liftM Just $ newLoggerWithCustomErrorFunction f fp
maybeSpawnLogger _ _                  = return Nothing
```

```haskell
newLoggerWithCustomErrorFunction :: (ByteString -> IO ())
                                 -> FilePath   -- ^ log file to use
                                 -> IO Logger
newLoggerWithCustomErrorFunction errAction fp = do
    q  <- newIORef mempty
    dw <- newEmptyMVar
    th <- newEmptyMVar

    let lg = Logger q dw fp th errAction

    mask_ $ do
      tid <- forkIOLabeledWithUnmaskBs "snap-server: logging" $
               loggingThread lg
      putMVar th tid

    return lg
```

[Source](https://github.com/snapframework/snap-server/blob/a539ac3dc7eafdff5e61b29591bc81fd9976b529/src/System/FastLogger.hs#L74)


### Don't use `mask_`

In most cases it is wrong, so don't use it unless you exactly know what you
are doing. If `newLoggerWithCustomErrorFunction` action is called without
async exceptions masked, then it will leak resource. It is too late to mask
async exceptions here. Probably it should be stated in documentation to the
function, but actually it is clear enough -- it should be used only inside
`bracket` or similar context. There is nothing wrong with `mask_` here, it is
just unnecessary. But it gives you false feeling of safety, and can be a sign
of other issues.

One can think that `mask_` here is useful because protects at least part of
the code from async exceptions. But that is not true, async exceptions will be
postponed till end of `mask_`, but then will be delivered anyway (unless
masked on caller site). The result is the same -- orphan thread.


### Other issues

Lets identify all points where something can go wrong. First of all, the
resource here is a thread, so we should make sure it doesn't become orphan on
failure. Everything above `forkIOLabeledWithUnmaskBs` is irrelevant, because
the thread is not yet created here. The only point if failure is `putMVar`.
To be carefull we should protect `forkIOLabeledWithUnmaskBs` against failure in
`putMVar`, but it is not necessary.

From documentation it is clear, that it will not throw async exception unless
the `MVar` is full. Here it is empty for sure. (Note: it is documented not in
`Control.Concurrent.MVar`, but in `Control.Exception`.)

Unfortunately the documentation for `putMVar` doesn't say anything about sync
exceptions. (And it can throw at least `BlockedIndefinitelyOnMVar`, but not in
our case.) But it is very common to rely on it not to throw sync exception, so
nobody will break that assumption ever. We can think about it as a
documentation bug.

Conclusion: the function is safe.


## example #4

```haskell
-- | Kills a logger thread, causing any unwritten contents to be
-- flushed out to disk
stopLogger :: Logger -> IO ()
stopLogger lg = withMVar (_loggingThread lg) killThread
```

Lets remember the contract: function should release resource even in case of
failure. Well, it is not always possible, but it should do it's best.

There is two `IO` actions here, `withMVar` and `killThread`. Both are
potential points of failure.

Documentation for `withMVar` states:

> it is only atomic if there are no other producers for this MVar.

The wording probably can be better. But it will not throw if the `MVar` is
full initially and nobody tries to put anything into it until exit from
`withMVar`. In out case it is true -- the mvar is not used anywhere after
spawning the thread. But we need to inspect the code around to find that out,
so it would be better to document the design.

`killThread` is problematic though. AFAIK it can't throw sync exceptions, but
it can throw async exception even when wrapped into `uninterruptibleMask`
(!!!). Also it is interruptible even when doesn't block. You should read it's
documentation carefully before using it.

In this particular case it is enough to wrap `killThread` into
`uninterruptibleMask`. Other option is to provide eventual guarantees, e.g.
spawn other thread to do `killThread` asynchronously:

```haskell
stopLogger lg = withMVar (_loggingThread lg) $ \threadId ->
  killThread threadId `onException` void (forkIO $ killThread threadId)
```

Here the `ThreadId` of the helper thread is unknown to anybody else, so we can
be sure nobody will send async exception to it.


## example #5

```haskell
runLoops = E.bracket (mapM newLoop [0 .. (nLoops - 1)])
                     (mapM_ killLoop)
                     (mapM_ waitLoop)
```

[Source](https://github.com/snapframework/snap-server/blob/a539ac3dc7eafdff5e61b29591bc81fd9976b529/src/Snap/Internal/Http/Server/Session.hs#L132)

Do you see the issue here?

Never assume anything. What if one of `newLoop` fails? Then all already
created loops will leak. What if one of `killLoop` fails? Then all subsequent
loops will not be killed.

Note: It is possible that `newLoop` and `killLoop` never throw (not in our
case though), but the principle of local reasoning forces us to handle
exceptions here or at least write a comment. We probably should fold the `killLoop`s
with `finally`. Something like `ResourceT` can simplify this case a lot.


## Final notes

Internally `snap-server` does a lot of non-local manipulations with mask state.
It is hard to extract any meaningful example because of non-locality. I'll
simply provide a [link](https://github.com/snapframework/snap-server/blob/a539ac3dc7eafdff5e61b29591bc81fd9976b529/src/Snap/Internal/Http/Server/TimeoutManager.hs#L169)
The `restore` seems to be changing masking state, but it is not clear how it
affects the code. Probably there are good reasons for such design though.
Also `eatException` is used a lot, and that is a bad sign.

`bracket` is not the only source of errors, but the same approach can be
applied to most cases.

I didn't have a goal to uncover all issues with exception handling here. And
I'm sure there are cases much harder to find and fix. But in most cases you'll
avoid a lot of mistakes if you follow the basic principles:

* Don't assume anything

* Keep it simple

* Divide and conquer

* Reason locally


Thanks to all authors of `snap-server` for excellent library, I really appreciate your work.

