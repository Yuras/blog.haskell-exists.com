---
title: Handling (async) exceptions in haskell: pushing bracket to the limits
published: 2014-11-20
---

(Originally posted [here](https://github.com/Yuras/io-region/wiki/Handling-%28async%29-exceptions-in-haskell:-pushing-bracket-to-the-limits))

Probably `bracket` is the most widely used approach to resource management in
haskell. There are [other][1], probably more convenient, but `bracket` covers
most of use cases. However it is common to claim, that haskell is broken with
respect to exceptions. Here we'll try to play with `bracket` and it's
implementation in order to find out, what exactly is wrong with exceptions in
haskell and how to fix them. I'll assume you know how `bracket` is implemented
and why (see [here][2] for details) and understand basic principles of
exception handling.


The post is organized into a series of examples, each subsequent extends the
previous one.


## example #1: Playground

To study internal implementation of `bracket` we'll need a convenient way to
test it. For example, we'll need to run sample code and check whether any
resource leaked. Lets use file I/O as a model and create custom `Handle`-like
API

First, imports (I'll usually skip unimportant details, but link to full source
code at the end of section):

```haskell
import Data.IORef
import Control.Exception (SomeException)
import qualified Control.Exception as E
import System.IO.Unsafe (unsafePerformIO)
```

To account allocated resources, lets use global variable. Right now we care
only about total number of live handles:

```haskell
{-# NOINLINE numHandles #-}
numHandles :: IORef Int
numHandles = unsafePerformIO $ newIORef 0
```

The `Handle` and basic operation:

```haskell
data Handle = Handle

openFile :: FilePath -> IO Handle
openFile _ = do
  modifyIORef' numHandles succ
  return Handle

hClose :: Handle -> IO ()
hClose _ = modifyIORef numHandles pred

hPutStr :: Handle -> String -> IO ()
hPutStr _ _ = return ()
```

Note, that `openFile` simply increments global resource counter, and `hClose`
decrements it.

We also want to know whether our sample throws an exception and how many
resources leaked. The following helper executes sample action and outputs any
exception and content of the resource counter on exit:

```haskell
test :: IO () -> IO ()
test action = do
  action `E.catch` \e -> do
    putStrLn $ "exception: " ++ show (e :: SomeException)
  readIORef numHandles >>= putStrLn . ("Number of open handles: " ++) . show
```

That is all for now. Lets test it with the following example:

```haskell
main :: IO ()
main = test example

example :: IO ()
example = do
  h <- openFile "path"
  hPutStr h "Hello"
  hClose h
```

It is obviously not exception-safe, but if you run it, you'll see the next:

> Number of open handles: 0

No resource leak, and that it great. But what if something goes wrong before
we close the handle?

```haskell
example = do
  h <- openFile "path"
  hPutStr h "Hello"
  error "something went wrong"
  hClose h
```

> exception: something went wrong
>
> Number of open handles: 1

So the exception escaped to top level, and the handle leaked, as expected. You
can find full source code
[here](https://github.com/Yuras/io-region/blob/master/misc/imask/example1.hs)


## example #2: bracket

The issue in the example #1 is pretty clear -- we should ensure the resource
is freed even in case of exception. That is exactly the use case `bracket`
exists for:

```haskell
bracket :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
bracket allocate release use =
  E.mask $ \restore -> do
    resource <- allocate
    restore (use resource)
      `E.finally` release resource

example =
  bracket (openFile "path") hClose $ \h -> do
    hPutStr h "Hello"
    error "something went wrong"
    hPutStr h "World"
```

(Here the implementation of `bracket` follows the standard implementation.) Try
it and check that the resource doesn't leak. It looks good enough. It even
handles asynchronous exceptions (You if are not sure, please read [more][2]).
Lets check that. The easiest way is to use `timeout` to raise async exception
and `threadDelay` to make sure it will be delivered exactly in the point we
need
it. Here is an example:

```haskell
import System.Timeout (timeout)

example = void $ timeout (1 * 1000 * 1000) $
  bracket (openFile "path") hClose $ \h -> do
    hPutStr h "Hello"
    hPutStr h "World"
    threadDelay (2 * 1000 * 1000)
```

Note: both functions accept time in nanoseconds. We choose timeout to be 1 sec,
and the delay is 2 secs. That way we ensure the async exception will interrupt
the delay. Try it yourself to see that it finishes in ~1 sec without exception
or resource leak.

But what if the exception will interrupt cleanup action, `hClose`?  First of
all, `hClose` is interruptible action -- it can be interrupted by async
exception even if they are masked. Why? For example, because it needs to flush
internal buffers before closing the file. Lets model that:

To track, what was actually written to file, we'll flush data to global
`IORef`.

```haskell
{-# NOINLINE dataWritten #-}
dataWritten :: IORef [String]
dataWritten = unsafePerformIO $ newIORef []
```

`Handle` will have internal buffer, and `hFlush` will move it's content (if
any) to the `dataWritten` variable:

```haskell
data Handle = Handle (IORef (Maybe String))

hFlush :: Handle -> IO ()
hFlush (Handle ref) = do
  val <- readIORef ref
  case val of
    Just str -> modifyIORef dataWritten (str :)
    _ -> return ()
  writeIORef ref Nothing

```

Now the `hPutStr`. It will flush the old buffer and replace it with new value:

```haskell
hPutStr :: Handle -> String -> IO ()
hPutStr h@(Handle ref) str = do
  hFlush h
  writeIORef ref (Just str)
```

And finally `hClose`. If flushes the buffer and closes the handle. Our
implementation roughly models the implementation from `ghc`:

```haskell
hClose :: Handle -> IO ()
hClose h = hFlush h
  `E.finally` modifyIORef numHandles pred
```

Note: `hClose` guarantees, that the handle will be closed even if `hClose`
fails (see
[docs](http://hackage.haskell.org/package/base-4.7.0.1/docs/System-IO.html#v:hClose)).
Here we use `finally` to make sure resource doesn't leaks even if `hFlush`
fails.

Note2: the current implementation in `ghc` is a bit broken -- there are rare
cases where `hClose` fails to free resource. This issue is well known and easy
to fix (but nobody cares enough even to open a ticket :) ), We'll ignore that
unfortunate fact.

Lets also print content of `dataWritten` on exit:

```haskell
test action = do
  action `E.catch` \e -> do
    putStrLn $ "exception: " ++ show (e :: SomeException)
  readIORef numHandles >>= putStrLn . ("Number of open handles: " ++) . show
  readIORef dataWritten >>= putStrLn . ("Data writtern to file: " ++) . show
```

If you run the last example in the new playground, you'll see the output:

> Number of open handles: 0
>
> Data writtern to file: ["World","Hello"]

Looks good. Now we can check how it works if `hFlush` fails on cleanup:

```haskell
hClose :: Handle -> IO ()
hClose h = hFlushFailing h
  `E.finally` modifyIORef numHandles pred

hFlushFailing :: Handle -> IO ()
hFlushFailing _ = error "hFlush failed"
```

We replace `hFlush` with it's failing variant. Note: `hPutStr` uses the
initial version still. The example is the same:

```haskell
example = void $ timeout (1 * 1000 * 1000) $
  bracket (openFile "path") hClose $ \h -> do
    hPutStr h "Hello"
    hPutStr h "World"
    threadDelay (2 * 1000 * 1000)
```

> exception: hFlush failed
>
> Number of open handles: 0
>
> Data writtern to file: ["Hello"]

Exception escapes to top level, but the resource was released correctly. Also
some data was flush to file, but other wasn't. That is ok, basic exception
safely guarantees are satisfied: we left the file in valid (closed in that
case) state. The state is probably unknown, because we don't know exactly what
was written to the file, but that is ok. We don't really care about file
content here because the whole operation fails anyway.

So, everything is good? `timeout` uses async exception to interrupt the
action, but catches it internally, and the program can continue execution. In
our case, async exception interrupted `threadDelay`, but `bracket` catches the
exception and run cleanup action. Cleanup fails, throwing other exception. Now
`bracket` has to decide what to do -- rethrow the original exception or let the
new one to propagate. The standard implementation chooses the later solution,
it lets new exception propagate. The async exception was silently ignored. That
is the instance of double-throw issue.

Homework: construct double-throw issue not using async exceptions.

[The source code so
far.](https://github.com/Yuras/io-region/blob/master/misc/imask/example2.hs)


## example #3: fixing `bracket`

The previous example was interrupted by timeout, so it already failed, and we
don't care about file's content anymore. We want to prevent resource leak,
that is all. There is no any reason to propagate exception from cleanup
action here. So lets fix `bracket`:

```haskell
bracket :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
bracket allocate release use =
  E.mask $ \restore -> do
    resource <- allocate
    restore (use resource) `E.finally`
      (E.catch
        (release resource)
        (\e -> putStrLn $ "Ignoring: " ++ show (e :: SomeException)))
```

We guarded cleanup action with `catch` and ignore any exceptions. If you run
the example, you'll see:

> Ignoring: hFlush failed
>
> Number of open handles: 0
>
> Data writtern to file: ["Hello"]

That makes perfect sense, but totally wrong. Lets remove the `timeout`:

```haskell
example =
  bracket (openFile "path") hClose $ \h -> do
    hPutStr h "Hello"
    hPutStr h "World"
    threadDelay (2 * 1000 * 1000)
```

> Ignoring: hFlush failed
>
> Number of open handles: 0
>
> Data writtern to file: ["Hello"]

The exception is ignored. Note the difference, now the example succeeded, but
internal buffer was not flushed. We silently lost data. We should ignore
exception only if the inner action fails:

```haskell
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket allocate release use =
  E.mask $ \restore -> do
    resource <- allocate
    result <- E.catch (restore $ use resource) $ \e -> do
      void (release resource) `E.catch` \e' ->
        putStrLn ("Ignoring exception: " ++ show (e' :: SomeException))
      E.throw (e :: SomeException)
    void (release resource)
    return result
```

Try this with and without the `timeout`.
[The source code]
(https://github.com/Yuras/io-region/blob/master/misc/imask/example3.hs)


## example #4: kill them all

The implementation is still problematic. To show that, lets try to kill the
main thread while it is executing the example:

```haskell
hClose :: Handle -> IO ()
hClose h = hFlushFailing h
  `E.finally` modifyIORef numHandles pred

hFlushFailing :: Handle -> IO ()
hFlushFailing _ = do
  error "hFlush failed"

example :: IO ()
example = void $ timeout (2 * 1000 * 1000) $
  bracket (openFile "path") hClose $ \h -> do
    hPutStr h "Hello"
    hPutStr h "World"
    threadDelay (2 * 1000 * 1000)

main = do
  pid <- myThreadId
  void $ forkIO $ do
    threadDelay (1 * 1000 * 1000)
    killThread pid
  test example
```

> Ignoring exception: hFlush failed
>
> exception: thread killed
>
> Number of open handles: 0
>
> Data writtern to file: ["Hello"]

The delay is interrupted by `killThread` (it used async exception for that).
`hClose` failed to flush buffers, but the exception was ignored, and the
thread was successfully killed. Perfect.

Now lets give `timeout` a chance to fire. We can add delay in `hFlushFailing`,
so that it will block waiting for the timeout. Obviously async exception will
be ignored, just like in previous case with sync exception. But what if we
change the order? Let timeout fire before killing thread:

```haskell
hFlushFailing :: Handle -> IO ()
hFlushFailing _ = do
  threadDelay (2 * 1000 * 1000)
  error "hFlush failed"

example :: IO ()
example = void $ timeout (1 * 1000 * 1000) $
  bracket (openFile "path") hClose $ \h -> do
    hPutStr h "Hello"
    hPutStr h "World"
    threadDelay (2 * 1000 * 1000)

main = do
  pid <- myThreadId
  void $ forkIO $ do
    threadDelay (2 * 1000 * 1000)
    killThread pid
  test example
```

> Ignoring exception: thread killed
>
> Number of open handles: 0
>
> Data writtern to file: ["Hello"]

We failed to kill the thread! It is probably clear now, that there is no way
for `bracket` to choose what exception to propagate. Either way will break
one of this two examples. What makes the difference compared to sync
exceptions? Async exceptions are external, so neither `hClose` no `bracket`
know their semantics, and have to be exception-neutral to them: always
propagate instead of handling or ignoring. But clearly there is no way to
propagate two exceptions at once.

[Source
code](https://github.com/Yuras/io-region/blob/master/misc/imask/example4.hs)


## example #5: uninterruptibleMask

We just showed that async exceptions are problematic when come paired. Then we
should disable them using `uninterruptibleMask`. Lets try the previous
example:

```haskell
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket allocate release use =
  E.mask $ \restore -> do
    resource <- allocate
    result <- E.catch (restore $ use resource) $ \e -> do
      -- I added uninterruptibleMask below:
      void (E.uninterruptibleMask_ $ release resource) `E.catch` \e' ->
        putStrLn ("Ignoring exception: " ++ show (e' :: SomeException))
      E.throw (e :: SomeException)
    void (release resource)
    return result

hFlushFailing :: Handle -> IO ()
hFlushFailing _ = do
  -- delay set to 20 seconds to make the issue clear
  threadDelay (20 * 1000 * 1000)
  error "hFlush failed"

```

> $ time ./example5
>
> Ignoring exception: hFlush failed
>
> exception: thread killed
>
> Number of open handles: 0
>
> Data writtern to file: ["Hello"]
> 
> real	0m21.017s
>
> user	0m0.000s
>
> sys	0m0.004s

OK, nobody will say that we failed to kill the thread, because we finally
killed it. After 20 seconds of waiting....

Flushing buffers usually is fast. Until you are working with network
filesystem, or ftp server via FUSE, or TCP socket, or.... But if you are only
using the local filesystem, then you are probably OK :)

[Source
code](https://github.com/Yuras/io-region/blob/master/misc/imask/example6.hs)


## example #6: pushing to the limits

Double throw issue is unsolvable in current haskell without introducing
unbound blocking in cleanup actions. I hope the examples convinced you. Is
haskell broken? Probably. Is there anything we can do? Definitely.

Lets think about the semantics. We are going to kill the thread, but we can't
because we are blocked on `hFlush`. Note that we *don't need* to flush buffers
here, we are not interested in side effect on the action. We just want to kill
it. We are blocked on the action, that we don't need at all. We only want to
ensure no resources leak.

(From theoretical point of view we *should not* flush buffers here because in
case of failure we should minimize side effects of our action. Here flushing
buffers is unnecessary side effect. Ideally we should undo all writes to the
file to achieve strong exception safety, but unfortunately it is not possible
here, so we minimize side effects and guarantee basic exception safety, see
[here][5])

`hClose` provides a way to stop it from doing unnecessary work: it is
interruptible, but guarantees resource cleanup even in case of exception. But
to interrupt action we need async exception, which is harmful because of
double throw. We need a way to disable async exceptions but interrupt any
interruptible action on the way. Something like a fast way that skips all
optional work. Lets call it `interruptibleMask`.

IMO it is not possible to properly implement it in user space, minimal support
from RTS is necessary here. But the
[example6](https://github.com/Yuras/io-region/blob/master/misc/imask/example6.hs)
contains pure mans implementation, so that you can try it out, play with it
and get the idea. No resource leaks, `KillThread` exception is not ignored,
there is no double-throw issue. (The implementation is not thread safe and
pretty limited, so you'll probably break it easily.)


## Notes: `uninterruptibleMask` in `bracket` proposal

(See also [previous][4] [discussion][5])

The idea of the post is 2 or 3 month old. I came to the double throw issue
[half a year ago][3]. Unfortunately nobody answered my questions, either
nobody knows about the issue or nobody cares.

So the post is not directly related to the recent proposal to introduce
`uninterruptibleMask` in `bracket`, but the proposal forced me to publish this
writeup earlier. I didn't have time to ensure it is sound and doesn't contain
flaws. So I assume it does.

But I hope to convince people that the design space is pretty wide right now,
there can be much better solutions to exception handling in haskell. The
proposal is probably a local optimum, but it is hard to revert later because
it will break correct code. There will be no way to escape the local optimum
even if we find better one. We should explore the design space before closing
the door.

Thanks to Eyal Lotem for very useful discussion.


## withFile (later addition)

I forgot to describe one important idea, but it doesn't fit to the post naturally
anymore. So I'll just append it to the end.

The semantics of the example6 will not change if you move `hFlush` from `hClose`
to the end of bracket's 3d argument and use ordinary `uninterruptibleMask` in
cleanup. So `withFile` will look like the next:

``` haskell
withFile :: FilePath -> (Handle -> IO a) -> IO a
withFile path action =
  bracket (openFile path)
          (uninterruptibleMask . hClose)
          (\h -> do
             r <- action h
             hFlush h
             return r)
```

That approach doesn't require any change in compiler, but is harder to adopt
widely because it will break a lot of code.

[1]: overview-of-resource-management-in-haskell.html

[2]: http://www.well-typed.com/blog/97/

[3]: http://haskell.1045720.n5.nabble.com/Control-Exception-bracket-is-broken-td5752251.html

[4]: https://www.haskell.org/pipermail/libraries/2014-November/024231.html

[5]: handling-async-exceptions-in-haskell-the-right-way.html

