---
title:  Haskell pdf-toolbox: new release, future API changes and design mistakes
published: 2015-03-06
---

As you probably don't know, I'm working on a PDF processing library
in Haskell, [pdf-toolbox](https://github.com/Yuras/pdf-toolbox).
It can parse PDF files, generate them, it supports encrypted files, it can
do incremental updates, extract text, blah-blah-blah. If you are interested,
let me outline the current state and my future plans.

# New release

I just released new versions for all packages that belong to the library:
[core](https://hackage.haskell.org/package/pdf-toolbox-core)
, [document](https://hackage.haskell.org/package/pdf-toolbox-document)
, [content](https://hackage.haskell.org/package/pdf-toolbox-content)
and [viewer](https://hackage.haskell.org/package/pdf-toolbox-viewer).

The main change: `pdf-toolbox-document` now supports encryption handler
version 4, so it can handle files encrypted with `AES` algorithm.
Also few bugs were fixed in `core` and `content`. As you can see, nothing
terribly exciting, and the blog post is not about the release actually.

# Current state of the HEAD

At the end of 2014 I started API rewrite. I mostly finished with the initial
goals, though more work is necessary to fix viewer and examples to compile
again. But when doing the rewrite I fount that the API is still far from
ideal. Well, it is _bad_. That is why I decided to port the latest changes
to stable branch and release it, and then continue working on API redesign.

# Broken API

I'm still not 100% sure what to do, and I'd like to know your opinion. Here
the current plan.

## PDF types

The design of
[PDF types](https://hackage.haskell.org/package/pdf-toolbox-core-0.0.3.0/docs/Pdf-Toolbox-Core-Object-Types.html#t:Object)
is terrible. [Newtype over bool](https://hackage.haskell.org/package/pdf-toolbox-core-0.0.3.0/docs/Pdf-Toolbox-Core-Object-Types.html#t:Boolean)?!! Yes, it was me who wrote that, but I did it more then 3 years ago,
and I have no idea why I did that:

```haskell
data Object a =
  ONumber Number |
  OBoolean Boolean |
  OName Name |
  ODict Dict |
  OArray Array |
  OStr Str |
  OStream (Stream a) |
  ORef Ref |
  ONull
  deriving (Eq, Show)

newtype Boolean = Boolean Bool
  deriving (Eq, Show)

newtype Dict = Dict [(Name, Object ())]
  deriving (Eq, Show)

-- and so on...
```

I'm going to remove most (all?) of the newtypes and introduce `HashMap` and
`Vector` instead of lists.

Also, right now `Stream` type has a payload. It could be an actual stream
content, or just an offset of the content, or anything else. Sometimes it is
convenient, but now I think that it was bad idea. I'm going to remove
the payload completely and pass it separately when necessary.

Probably something like that:

```haskell
data Object =
  Number Scientific |
  Bool Bool |
  Name Text |
  Dict (HashMap Text Object) |
  Array (Vector Object) |
  String Text |
  Stream (HashMap Text Object) |
  Ref (Int, Int) |
  Null
  deriving (Eq, Show)
```

Also a better way to convert PDF values to domain types is necessary. Probably
something like a [Parser](http://hackage.haskell.org/package/aeson-0.8.0.2/docs/Data-Aeson-Types.html#t:Parser),
that is used in `FromJSON` type class in `aeson`?

## Error handling

The stable version uses [EitherT](https://hackage.haskell.org/package/pdf-toolbox-core-0.0.3.0/docs/Pdf-Toolbox-Core-Error.html)
to handle errors. I believe that it was wrong design decision. I switched to
extensible exceptions in HEAD already. It still requires cleanup, e.g. I
want to introduce more specific exceptions, but it is already much better in
my opinion.

## Custom monad transformer

I have `Pdf` monad transformer in [stable version](https://hackage.haskell.org/package/pdf-toolbox-document-0.0.4.0/docs/Pdf-Toolbox-Document-Pdf.html).
I even have a [type class](https://hackage.haskell.org/package/pdf-toolbox-document-0.0.4.0/docs/Pdf-Toolbox-Document-Monad.html)
for PDF operations.
That is probably the worst thing one may do for PDF library.
Just imaging that you want to operate on two PDF files at the same time. With
custom monad it becomes a pain, and the easiest solution is probably to
fork a separate thread for each PDF file.

In HEAD I already switched to `IO`, now all objects are passed explicitly as
values. I like it :)

Side note: I like that a lot of libraries on Hackage switched to extensible
exceptions and replaced custom monads with `IO` in API. E.g. websockets and
mongoDB packages.

## Reading encrypted documents

The current design is an example of unsafe API. You should check that the
document is encrypted and then set user password
, see [here](https://hackage.haskell.org/package/pdf-toolbox-document-0.0.4.0/docs/Pdf-Toolbox-Document-Pdf.html#v:isEncrypted).
The problem occurs when you forgot to do that -- you get strange errors
somewhere else, e.g. when extracting text.

I can add a special check to each operation and throw `DocumentEncrypted`
exception when user forgot to set a password. But that will slowdown
everything. And I can't require password before opening the document because
user should be able to examine parts of the document (encryption dictionary)
to decide what password to use. I'm still looking for better solution.

## Abstractions and tests

PDF is very big and complex. It is hard to come with solid abstractions
because you can't keep everything in your mind. A month ago I was sure I have
excellent separation between PDF as a file format; as a collection of
values; and as a document with title, pages etc. But one day my naive mental
picture was ruined because of interconnections I was not aware about.

What can I do with that? Rearrange abstractions? Drop them? I don't want to
decide upfront, I'm going to be more agile instead. And refactoring will
become the most important tool, but I need more tests for that. So test
coverage becomes an important goal for me.

# Conclusion

I wrote the first version of PDF parser more then 3 years ago. It was a
dirty prototype, but it did the work -- it was a tool to examine PDF file
internal structure. Now it is a library with a lot of features and long
history. It is not a prototype anymore, but it is dirty still :)

I don't know how many users the library has. I get bug reports and feature
requests periodically, but the only direct user is `hoodle-publish` package.
As I described above, I'm going to completely change the API, and I'd like to
know your opinion if you are using, going to use or even not going to use it.
