---
title: Effects encoded in types break encapsulation
date: 2015-05-17
---

One of the most important Haskell features for me is a clear separation of pure
and impure code. I can say whether a function can have side effects only by
looking to its type. For example, `print` can perform arbitrary side effect,
while `(++)` can't perform any:

```haskell
print :: Show a => a -> IO ()
(++) :: [a] -> [a] -> [a]
```

# Restricted side effects

But sometimes such the binary separation (pure vs impure) is not enough, we
may need fine grained control on what side effects a function may perform.
Lets quote [RWH](http://book.realworldhaskell.org/read/programming-with-monads.html#id648782)

> The blessing and curse of the IO monad is that it is extremely powerful.
> If we believe that careful use of types helps us to avoid programming
> mistakes, then the IO monad should be a great source of unease. Because the
> IO monad imposes no restrictions on what we can do, it leaves us vulnerable
> to all kinds of accidents.
>
> How can we tame its power? Let's say that we would like to guarantee to
> ourselves that a piece of code can read and write files on the local
> filesystem, but that it will not access the network. We can't use the plain
> IO monad, because it won't restrict us.

And Haskell provides us with tools to restrict side effects to a known set.
For example, we can define custom type class, that restricts effects to
HTTP operations (we are ignoring some details, like error handling etc.):

```haskell
class Monad m => MonadHttp m where
  get :: Url -> m ByteString
  post :: Url -> ByteString -> m ByteString
```

Lets suppose we want to fetch weather information. It can look like the next:

```haskell
fetchWeather :: MonadHttp m => m Weather
fetchWeather =
  parseWeather <$> get "http://example.com/weather.json"
```

Note that `MonadHttp` doesn't have `MonadIO` instance, so `fetchWeather` can't
perform any side effects except HTTP request. So far so good.

But later we decide to cache weather information in file. How can we do that?
Lets define another type class for filesystem operations:

```haskell
class Monad m => MonadFS m where
  readFile :: FilePath -> m (Maybe ByteString)
  writeFile :: FilePath -> ByteString -> m ()
```

Then `fetchWeather` function will try to read weather information from a file,
and perform HTTP request only if the file doesn't exists:

```haskell
fetchWeather :: (MonadHttp m, MonadFS m) => m Weather
fetchWeather = do
  json <- do
    cached <- readFile "cache.json"
    case cached of
      Just json -> return json
      Nothing -> do
        json <- get "http://example.com/weather.json"
        writeFile "cache.json" json
        return json
  return (parseWeather json)
```

Note that the function can perform two types of side effects: make HTTP
requests and access filesystem, and it is clearly reflected by its type.
Excellent!

# Encapsulation

But lets look from point of view of function's users. After introducing the
cache, they have to update their code to introduce new type constraint,
`MonadFS`. That is not a big deal when we control all the client code,
but what if we have a lot of downstream dependencies? People will be angry
because we broke their code.

The main question is why our users even noticed the change? Introducing cache
should be transparent for them because it is actually an implementation detail.
People just want to get weather forecast, they don't want to care where we
get weather information from. Implementation details should be encapsulated,
but `fetchWeather` leaks them via type classes for restricted effects.

The best way to encapsulate effects is not to restrict them. We should provide
users with `fetchWeather` function that works in unrestricted `IO` monad:

```haskell
fetchWeatherIO :: IO Weather
```

That way we are free to change its implementation and introduce other side
effects without affecting users, e.g. we may cache weather information in
database or global mutable variable.

# Interface vs implementation

But that doesn't mean that restricting side effects always is bad. It is just
a tool, that can be useful sometimes, but it is not a silver bullet. Most of
the time it should not be used in public interface, but it is good to have in
implementation. The point of the article is that we should be careful when
exposing side effects to users unless it is desired.

# On exceptions

It is interesting that the same arguments could be applied to exceptions vs
`ExceptT` discussion (see [here](http://www.reddit.com/r/haskell/comments/35sk6w/best_practices_for_using_exceptions_an_fp/)).
A list of all exceptions function may throw (or a list of failures in error
sum type) is actually an implementation detail. That sound counterintuitive,
but we don't actually need to handle all exceptional cases, so we don't need
a gigant sum type for them.
