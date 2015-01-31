---
title: Overview of resource management in Haskell
published: 2014-10-04
---

(Originally posted [here](https://github.com/Yuras/io-region/wiki/Overview%20of%20resource%20management%20in%20Haskell))

Writing exception safe code is hard in any language. But in Haskell it is
especially hard because of asynchronous exceptions.

## Problem: How to ensure all resources will be freed?

File descriptors, tcp ports, memory etc. They are limited resources. If you are
not careful enough, you'll run out of them sooner of later. For long running
applications it is mission critical to free resources as soon as possible.

## Solution 1: Manually free resources

```haskell
do
  resource <- allocate
  use resource
  free resource
```

Allocate resource, use it and free it. Very easy but completely wrong. First of
all, it is too easy to forget to free resource. But even if you never forget
anything, you are not safe enough -- an exception can abort normal control flow
leaving resources alive forever.

## Solution 2: finally

```haskell
do
  resource <- allocate
  use resource
    `finally` free resource
```

Much better -- the resource will be freed even if `use` throws exception.
Probably that is OK in most languages, but it is still wrong in Haskell.
Asynchronous exception can be raised in small window between `allocate` and
`finally` or between `finally` and `free`.

## Solution 3: mask asynchronous exceptions

```haskell
mask $ \restore -> do
  resource <- allocate
  restore (use resource)
    `finally` free resource
```

The idea is pretty clear -- disable asynchronous exceptions in critical parts
of code. This idiom is the most widely used. It is even implemented in standard
library under `bracket` name.

The solution is simple and robust, it works in almost all cases. But there are
few cases that don't fit the pattern well.

## Use case 1: free resource earler

Sometimes you may want to free resource before `bracket` does that:

```haskell
braket allocate free $ \resource -> do
  use resource
  if ...
    then free resource
    else ...
```

But there is no way to let `bracket` know that you already freed the resource.
As a result you have double free error.

## Solution 3: ResourceT

`resourcet` package provides monadic scope for deterministic resource
allocation. All resources will be freed on exit from the scope. Sounds similar
to `bracket`, but in addition it lets you free resource easer. Here is an
example:

```haskell
import qualified Control.Monad.Trans.Resource as R
R.runResourceT $ do
  (key, resource) <- R.allocate allocate free
  use resource
  if ...
    then R.release key
    else ...
```

Each resource is identified with unique key, that you can use to free the
resource earler and be sure release action will be called exactly once.

## Use case 2: let resource escape to outer scope

That's common pattern in c++: attach heap object to `auto_ptr` and release it
before return:

```c++
std::auto_ptr<Foo> createFoo() {
  auto_ptr<Foo> foo(new Foo);
  do_something(); // potentially throws exception
  return foo;
}
```

Here if `do_something` throws an exception, `auto_ptr` will delete the object
on exit from local scope. Otherwise `Foo` escapes to outer scope. Obviously it
is not possible with Haskell's `bracket` pattern. Probably you can do that with
`ResourceT`, but it is not supported out of the box.

## Soulution 4: type safe monadic regions

TODO: example

`regions` package provides type safe monad scope with ability to transfer
resource ownership to outler scope. In addition it uses black type level magic
to ensure that you can't return resource from the region without transfering
ownership (yes, it uses the same trick as `ST` monad.)

## Dynamic regions

There is a number of (probably rare) situations where neither solution can be
easily applied. For example, you may need to transfer resource to other thread;
or attach resources to an object (explicit owner); or combine a bunch of
resources into one umbrella resource to hide implementation details. What you
need here is dynamic regions that you can pass here and there, store in data
types, etc.

## io-region

The [package](https://github.com/Yuras/io-region) provides dynamic regions plus ability to free resources earler and
atomically transfer them between regions. It is not type safe in the same sence
as `region` package, but is much easer to use and understand.

### Exampes

```haskell
import Constrol.IO.Region (region)
import qualified Constrol.IO.Region as R

...
  region $ \r -> do
    resource <- R.alloc_ r allocate free
    use resource
    -- resource will be automatically freed here

...
  region $ \r -> do
    (resource, key) <- R.alloc r allocate free
    use resource
    if ...
      then R.free key  -- free it earler
      else use resource

...
  region $ \r1 -> do
    resource <- region $ \r2 -> do
      (resource1, key) <- R.alloc r2 allocate free
      use resource
      resource `R.moveTo` r1  -- transfer ownership to region r1
      return resource
    doSomethingElse resource
    -- resource will be freed here

...
  region $ \r1 -> do
    (r2, r2Key) <- R.alloc r1 R.open R.close  -- region is a resource too
    resource <- R.alloc r2 allocate free
    use resource
    r2Key `R.moveTo` r3  -- move region r2 ownership (and also the resource) to other region
```

