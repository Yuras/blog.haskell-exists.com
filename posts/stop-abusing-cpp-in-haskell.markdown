---
title: Stop (ab)using CPP in Haskell sources
published: 2015-02-01
---

## What is wrong with CPP ##

CPP is a *C* preprocessor, but it is common to use it in Haskell. That leads to
a number of issues.

- It can mess with haskell code.

CPP doesn't understand Haskell code, instead it assumes C code. It is free to
remove insignificant (for C, not for Haskell) whitespaces, expand macros in
Haskell comments and strings, mess with identifiers that contain `'` or `#`.

- It leads to [unnecessary recompilation](http://stackoverflow.com/questions/26785036/why-the-presence-absence-of-the-hscolour-binary-forces-to-recompile-the-quickche).

Every time you change your .cabal file, e.g. add new module, or update
dependencies, cabal regenerates `cabal-macros.h` file. Then recompilation
checker pessimistically decides to recompile all modules with CPP enabled.

- It makes automatic code analyzing and transforming harder.

If you use `hlint` or `HaRe`, then you probably know what I mean.

- When abused, it makes code harder to read.

It is not rare to see code intercalated with ifdefs that specify different
behaviour for different platforms of library versions. Sometimes that is
unavoidable though.

Most of the time CPP can be avoided or minimized. The most important tool here
it abstraction.

## Abstract over specific details ##

It is not Haskell specific, abstracting is widely used to minimize CPP in C.
When you need different behaviour based on current platform or version of some
dependencies, try to abstract over the difference instead of inlining platform
specific code.

At the first glance it may look impossible to do. In such cased I usually
simply duplicate code and then refactor it to reduce duplication.

Some times it is convenient to start with umbrella module that provides unified
interface for the rest of program, and a number of platform specific
implementations. Note: you don't need CPP to select specific module, cabal let
you conditionally include modules based on platform or other conditions.

## Example: fsnotify ##

Excellent example of such approach is [fsnotify](https://github.com/haskell-fswatch/hfsnotify)
package. It defines specific implementations for
[linux](https://github.com/haskell-fswatch/hfsnotify/blob/master/src/System/FSNotify/Linux.hs),
[osx](https://github.com/haskell-fswatch/hfsnotify/blob/master/src/System/FSNotify/OSX.hs) and
[win32](https://github.com/haskell-fswatch/hfsnotify/blob/master/src/System/FSNotify/Win32.hs),
and one [umbrella module](https://github.com/haskell-fswatch/hfsnotify/blob/master/src/System/FSNotify.hs).
A number of [other modules](https://github.com/haskell-fswatch/hfsnotify/tree/master/src/System/FSNotify)
contain common code, so duplication is really minimal.

Note that CPP is enabled only in the umbrella module for two reasons. First off all, it is used to import
specific implementation:

```haskell
#ifdef OS_Linux
import System.FSNotify.Linux
#else
# ifdef OS_Win32
import System.FSNotify.Win32
# else
# ifdef OS_Mac
import System.FSNotify.OSX
# else
type NativeManager = PollManager
# endif
# endif
#endif
```

That can be avoided too. To do that we can give the same name to platform
specific modules but move them into separate directories, `linux`, `osx` and
`win32`. Then manipulate `hs-source-dirs` field in cabal file to select correct
implementation. (Make sure to add other implementations to `extra-source-files`
to make sure `cabal sdist` will copy them into tarball.)

```haskell
-- in System.FSNotify:
import System.FSNotify.Platform

-- in fsnotify.cabal:
extra-source-files: linux/System/FSNotify/Platform.hs
                    osx/System/FSNotify/Platform.hs
                    win32/System/FSNotify/Platform.hs
hs-source-dirs: src
if os(linux)
  hs-source-dirs: linux
if os(darwin)
  hs-source-dirs: osx
if os(windows)
  hs-source-dirs: win32
```

The other use of CPP is to define `forkFinally` that is missing in older `base`:

```haskell
#if !MIN_VERSION_base(4,6,0)
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
#endif
```

The same technique can be used to avoid CPP here. I personally prefer to hide
such snippets into [custom prelude](https://github.com/Yuras/pdf-toolbox/blob/0732f15e8f73a724372d46670fa2d0d71d301650/core/lib/Prelude.hs)
and don't bother with `hs-source-path`.

(I don't think CPP should be avoided at all costs, I think that the amount of
CPP used in fsnotify is a good compromise. I just used it as a realworld
example how to avoid CPP.)

