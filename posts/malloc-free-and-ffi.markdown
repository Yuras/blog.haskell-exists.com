---
title: Malloc, free and FFI
published: 2015-02-08
---

TL;DR You should always free memory with the same allocator that allocated it
for you.


We'll discuss two different sets of `malloc` and `free` functions. The first
one is defined in `Foreign.Marshal.Alloc`, and the second one is part of C
runtime. To distinguish them, I'll use `H-malloc` and `H-free` names for
Haskell functions, and `C-malloc` and `C-free` for C functions.

Actually `H-malloc` and `H-free` just call their C counterparts, so usually
the same allocator is used in all this 4 functions. But that doesn't mean that
we can mix them. The documentation is clear:

```haskell
-- |Free a block of memory that was allocated with 'malloc',
-- 'mallocBytes', 'realloc', 'reallocBytes', 'Foreign.Marshal.Utils.new'
-- or any of the @new@/X/ functions in "Foreign.Marshal.Array" or
-- "Foreign.C.String".
--
free :: Ptr a -> IO ()
free  = _free
```

Note that it enumerates all the cases when `H-free` can be used, and `C-malloc` is
not listed here. There are two reasons for that. First of all, the
implementation may be changed to use some other allocator.

(You can skip this paragraph, it contains some low level details.) The second
reason is that sometimes your program happens to be linked with multiple
versions of C runtime. That sounds strange, but it is a very real
situation.  For example, your program may load external plugin statically
linked with C runtime other then yours. As a result you have three sets of
malloc/free functions: one from Haskell, another from your C runtime, and one
more from the plugin's C runtime. The last two are probably incompatible, and
you'll get random failures if you are not careful enough.

The usual rule to avoid any issue with allocator is: you should deallocate
memory in the same module where you allocated it. E.g. if you allocated memory
in Haskell, then please free it in Haskell. If you allocated memory in C
library, then please deallocate it in the same C library. (The same goes for
dynamically loaded plugins.)

The reason I wrote about it?  There is an
[issue](https://ghc.haskell.org/trac/ghc/ticket/9806) on `ghc` bug tracker
about replacing the allocator used in `H-malloc`. And I decided to check how
often code on github relies on the current behavior (e.g. uses `H-free` to deallocate
memory, that was allocated by `C-malloc`). I was surprised how common it is. Even
RWH recommends wrong approach:

```haskell
-- file: ch17/PCRE-compile.hs
compile :: ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
  useAsCString str $ \pattern -> do
    alloca $ \errptr       -> do
    alloca $ \erroffset    -> do
        pcre_ptr <- c_pcre_compile pattern (combineOptions flags) errptr erroffset nullPtr
        if pcre_ptr == nullPtr
            then do
                err <- peekCString =<< peek errptr
                return (Left err)
            else do
                reg <- newForeignPtr finalizerFree pcre_ptr -- release with free()
                return (Right (Regex reg str))
```

Here the `pcre_ptr` is allocated somewhere in pcre C library (probably using
`C-malloc`), and deallocated using `H-free` (via `finalizerFree`). This code
works most of the time, but it is wrong.  The correct approach would be to call
C function to deallocate memory. Some C libraries provide a special
function that guarantees to call the correct deallocator. In case of pcre
it seems to be `pcre_free`, and it should be used here instead of `H-free`.

