---
title: A beginners guide to API over-engineering
date: 2016-03-09
---

It is hard to [quantify over-engineering](https://www.reddit.com/r/haskell/comments/49i0ax/3_approaches_to_monadic_api_design_in_haskell/d0sbben).
But it is crucial for library API designer to identify over-engineering as early as
possible, and keep API simple and easy to use.

Over-engineering is about unnecessary complexity. In theory it is easy to avoid:
each time you see multiple alternatives and you are not sure which one
is better, you should select the simplest one. It practice we don't always
can say what alternative is simpler. Often we even don't see other
alternative, so the decision is made without any complexity analyze.

It this post I'll try to do a bit unusual thing. I'll take a problem, and make
the simples API design I can imaging. Then I'll make a series of redesigns,
successively increasing complexity, and we'll discuss whether
the complexity was necessary. You can decide yourself at which point
the API becomes "over-engineered".

The problem is question is a key/val database. The API we'll start looks
like the next:

```haskell
data DB
open :: FilePath -> IO DB
close :: DB -> IO ()
set :: DB -> ByteString -> ByteString -> IO ()
get :: DB -> ByteString -> IO (Maybe ByteString)
```

It is so simple, that I'm even not going to describe what it is doing.
One even don't need to know anything about monads to use it
(I assume that do-notation doesn't count). Now lets start iterating
on the API.

# Boilerplate

The first thing we notice is that three functions accept `DB` as an
argument. What is user uses this functions in a row, like this:

```haskell
withDB :: FilePath -> (DB -> IO a) -> IO a
withDB path use = bracket (new path) close use

withDB "some.db" $ \db -> do
  set db "key1" "val1"
  set db "key2" "val2"
  ...
  set db "keyN" "valN"
```

It is a boilerplate! We can avoid it using `Reader` monad transformer:

```haskell
set :: ByteString -> ByteString -> Reader DB IO ()
get :: ByteString -> Reader DB IO (Maybe ByteString)
```

Now user doesn't have to pass `DB` explicitly:

```haskell
withDB :: FilePath -> Reader DB IO a -> IO a
withDB path use = bracket (new path) close (runReader use)

withDB "some.db" $ do
  set "key1" "val1"
  set "key2" "val2"
  ...
  set "keyN" "valN"
```

Kind of cool. But also we increased complexity of the API. Now user has to know
about monads, transformers, lifting, etc. Even worse, we introduce new
abstraction, implicit environment, which is inadequate to out problem. Imaging
that you want two databases at the same time. With the initial version of
API it is trivial:

```haskell
withDB "some1.db" $ \db1 -> do
  withDB "some2.db" $ \db2 -> do
    maybe_v1 <- get db1 "key1"
    set db2 "key1" (fromMaybe "" maybe_v1)

    maybe_v2 <- get db1 "key2"
    set db2 "key2" (fromMaybe "" maybe_v2)
    ...
```

With the new API we'll need a bit of code golfing to do something like that.
Does a bit of boilerplate worse the complexity? How will users use the API
more often? As a library writers, we can't know, so it is better to stick
with simpler version, the initial one. A bit of boilerplate is [far cheaper
then wrong abstraction](http://www.sandimetz.com/blog/2016/1/20/the-wrong-abstraction).
But lets accept this additional complexity, and make the next step.

# MTL

At the previous step we introduced another issue. What if user uses his
own transformer stack? We can't compose different transformer stacks, we
can only nest them. Lets introduce `mtl`!

```haskell
set :: (MonadReader DB m, MonadIO m) => ByteString -> ByteString -> m ()
get :: (MonadReader DB m, MonadIO m) => ByteString -> m (Maybe ByteString)
```

Now API user has to understand type classes and `mtl`. Type signature
becomes longer. Seasoned haskellers will certainly not find it problematic,
but they are able to solve the issue with transformer stacks! By contrast
newcomers will probably not be able to use the API at all. Note that the
original design we started with doesn't suffer from the issue with
transformer stacks composition, simply because it doesn't use transformers.
Is the complexity necessary here? I think it is not, but you can decide
for yourself.

# Structured data

It is rare to store plain strings in database. Usually we store some structured
data, so we need a way to serialize and deserialize it. The standard way
to handle serialization is to use type classes:

```haskell
set :: (MonadReader DB m, MonadIO m, Serialize v) => ByteString -> v -> m ()
get :: (MonadReader DB m, MonadIO m, Serialize v) => ByteString -> m (Maybe v)
```

Looks good. Here I used `Serialize` type class from `cereal` package. Wait,
but what if user uses `binary` package? What about the next:

```haskell
set :: (MonadReader DB m, MonadIO m, Binary a) => ByteString -> a -> m ()
get :: (MonadReader DB m, MonadIO m, Binary a) => ByteString -> m (Maybe a)
```

Which one to use? Should we provide both variants? It will be really bad idea,
because it will introduce incidental dependencies on `cereal` and/or `binary`.
Should we introduce our own type class? Does it worse the complexity?
No, it doesn't. Serialization is not the core functionality for our library,
so our users should not be forced to learn one more serialization API just
to get a value from the database.

(A hint: key name sometimes is a structured data too, what about encoding and
decoding it too just like we do for values?)

# Effects

Suppose we want to count all operations we do on the database. It could be
useful for example for performance monitoring. We can just count everything
inside `MVar` and provide an API to access it:

```haskell
getPerformanceCounters :: DB -> IO Counters
```

But we should keep [effects under control](http://blog.haskell-exists.com/yuras/posts/effects-encoded-in-types-break-encapsulation.html)!
Lets use free monad to solve the issue:

```haskell
data Op next
  = Set ByteString ByteString next
  | Get ByteString (Maybe ByteString -> next)
  deriving (Functor)

interpret :: Free Op a -> IO a
```

Ops, we just moved the core functionality of our library out to interpreter. Now
the library does everything except writing to and reading from database.
I hope it is obviously over-engineering, so I'll stop right here.

# Conclusion

So monad transformers, free monads, etc. are bad and should be avoided? No, they
are cool and useful. But as any other tool, they have their own application
areas. There is nothing wrong in tranformer stack, `mtl` or free monad used in
implementation, but please think twice before exposing them to external API.
And certainly there are legitimate use cases where you really need them in API.
Design API thoughtfully and keep it simple.
