---
title: Namespaces, modules, qualified imports and a constant pain
published: 2015-02-27
---

This post is highly opinionated. It is about Haskell not having good namespace
story. I'll try to describe my point of view to this issue.

## T? M? BS?

What do this letters mean for you? Most likely they are `Data.Text`, `Data.Map`
and `Data.ByteString`. It is common to import this modules qualified and
introduce short aliases:

```haskell
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

foo = BS.length . BSL.toStrict . BSL.fromChunks . map T.encodeUtf8
```

It probably works for well known modules, but too often you can see `LM`, `I`,
`LT`, `ST` and so on. That becomes a tradition to use short names. But is it a
good tradition?

Short names make perfect sense in polymorphic code:

```haskell
catMaybes :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]
```

Here `a` and `x` can mean anything, so descriptive names will be misleading.

But it is not the case for module names. Short names here are confusing
because there is no common scheme, `T` can be used for `Data.Text` of
`Data.Traversable`. Is it so hard to type few letters?

```haskell
import qualified Data.ByteString as ByteString
-- Yes, you can have a dot in module name alias:
import qualified Data.ByteString.Lazy as Lazy.ByteString
import qualified Data.Text as Text

foo = ByteString.length
    . Lazy.ByteString.toStrict
    . Lazy.ByteString.fromChunks
    . map Text.encodeUtf8
```

It is definitely longer and a bit noisy, but at least it is unambiguous.

But it becomes a real pain, regardless of short vs long alias, when we have
long declaration (or a set of declarations) that use a lot of functions from
the same qualified module:

```haskell
import qualified Data.ByteString as ByteString

foo = ByteString.length
    . ByteString.append "!"
    . ByteString.drop 5
    . ByteString.take 10
    . ByteString.pack
```

Probably the best thing to do here is to refactor the declaration out to a
separate module and import `Data.ByteString` unqualified. But it would be
cool to be able to open particular module locally:

```haskell
import qualified Data.ByteString as ByteString

foo = length
    . append "!"
    . drop 5
    . take 10
    . pack
  where
  ByteString{..} = import ByteString
```

## somethingName, somethingEmail, somethingAge

Another common tradition is to prefix each record name or function with a
prefix to be able to use them unqualified:

```haskell
data Something = Something
  { somethingName :: Text
  , somethingEmail :: Email
  , somethingAge :: Int
  }

foo :: Something -> Text
foo something = "name:" <> somethingName something
```

Such a prefix is a poor mans namespace. Unfortunately in Haskell to create
named namespace, we have to create a module:

```haskell
module Something where

data Something = Something
  { name :: Text
  , email :: Email
  , age :: Int
  }

module Foo where

import Something (Something)
import qualified Something

foo :: Something -> Text
foo something = "name:" <> Something.name something
```

It is definitely better IMO. But it would be cool if datatype declaration will
introduce a namespace, so that we don't have to create a module:

```haskell
data Something = Something
  { qualified name :: Text
  , qualified email :: Email
  , qualified age :: Int
  }

foo :: Something -> Text
foo something = "name:" <> Something.name something
```

The syntax is terrible, I know. Probably there should be better syntax,
ideally we should be able to declare free functions withing the data type
namespace:

```haskell
data Person = Person
  { firstName :: Text
  , lastName :: Text
  }

  -- Note identation
  fullName :: Person -> Text
  fullName person = firstName person <> " " <> lastName person

foo person = "full name:" <> Person.fullName person
```

See also [nested modules](https://ghc.haskell.org/trac/ghc/wiki/Records/NestedModules)

## Type-directed name resolution, overloaded record fields, etc

I believe that code should be as unambiguous as it is possible. Adhoc
polymorphism makes code ambiguous -- you can't anymore say what function is
called only looking to it's usage side. Some recently proposed extensions has
their own value (e.g. I like anonymous records), but I believe that namespaces
support in Haskell should be improved, because namespaces is the right
solution for the described issues.

