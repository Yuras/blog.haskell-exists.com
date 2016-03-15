---
title: Turn GHC into a frontend for miniSTG
date: 2016-03-15
---

TLDR: [Here is a patch](https://github.com/Yuras/ghc/commits/miniSTG) for GHC to dump
STG in a form compatible with miniSTG.

# MiniSTG

The Spineless Tagless G-machine (STG) is an [abstract machine](http://research.microsoft.com/apps/pubs/default.aspx?id=67083)
designed to run lazy functional languages. An abstract code for it,
an STG language, is a minimal lazy functional language,
GHC [uses](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/StgSynType)
it as an intermediate representation for haskell code.

[MiniSTG](https://wiki.haskell.org/Ministg) is an interpreter for STG language.
It is a bit limited, [only integral literals](https://github.com/bjpop/ministg/blob/1908a9ad7653a95518473ba5f220e04f571989bc/src/Ministg/AST.hs#L31)
are supported, and [very few primops](https://github.com/bjpop/ministg/blob/1908a9ad7653a95518473ba5f220e04f571989bc/src/Ministg/AST.hs#L201)
are provided. But it is very useful if you want to study how
haskell code is executed by GHC. The most interesting its feature
is [execution tracing](https://wiki.haskell.org/Ministg#Execution_tracing).

To show few examples, lets define boxed integers:

```c
zero = CON(Int 0);

one = CON(Int 1);

plus = FUN(x y -> case x of {
	Int i -> case y of {
		Int j -> case plus# i j of {
			k -> let {
				r = CON(Int k);
				} in r;
		}
	}
});

main = THUNK(plus one one);
```

Here `Int` is a constructor for boxed integers (you don't have to define
constructors upfront), `zero` and `one` define boxed literals. The next declaration
is more interesting, `plus` is a function that takes two arguments. They are expected
to be boxed integers, and `case` is used to unboxed them. Then built-in primop
`plus#` performs the real work to add two unboxed integers. Finally the result
is reboxed and returned. The last line defines an entry point for the program.
The output:

```bash
$ ministg --noprelude -s EA main.stg
(Int 2)
```

Now lets define a list:

```c
nil = CON(Nil);

con = FUN(x xs -> let {
	r = CON(List x xs)
	} in r
);

length = FUN(l -> case l of {
	Nil -> zero;
	List x xs -> let {
		n = THUNK(length xs);
		} in plus one n;
});
```

Here `nil` is an empty list, `con` prepends a value to a list, and `length` calculates
length of a list using the `plus` function we already defined. Lets test it:

```c
list1 = THUNK(con zero nil);
list2 = THUNK(con zero list1);
list3 = THUNK(con zero list2);
list4 = THUNK(con zero list3);

main = THUNK(length list4);
```

Output:

```bash
$ ministg --noprelude -s EA main.stg
(Int 4)
```

I hope you got the idea. There are few restrictions: the only way to allocate anything on heap
is to use `CON`, `FUN` or `THUNK` inside `let` or on top level, so the next code is invalid
because it tries to allocate a list cell outside of `let` (compare with the definition above):

```c
con = FUN(x xs ->
	CON(List x xs)
);
```

Also all function arguments should be allocated on heap, you can't pass a complex expression
to a function. So the next code is invalid:

```c
length = FUN(l -> case l of {
	Nil -> zero;
	List x xs -> plus one (length xs);
});
```

MiniSTG doesn't support pattern matching on literals, so you have to use `eq#` and `intToBool#`
primops to compare boxed integers:

```c
eq = FUN(x y -> case x of {
	Int i -> case y of {
		Int j -> case eq# i j of {
			k -> intToBool# k
		};
	};
});

main = THUNK(eq one one);
```

Output:

```bash
$ ministg --noprelude -s EA main.stg
True
```

# GHC

Writing in STG is fun, but how real haskell code looks when translated into STG?
GHC has a `-ddump-stg` flag to dump it, but unfortunately it looks far from
what miniSTG accepts:

```c
f_rn0 :: GHC.Types.Int -> GHC.Types.Int
[GblId, Arity=1, Str=DmdType, Unf=OtherCon []] =
    sat-only \r [ds_s1Mn]
        case ds_s1Mn of wild_s1Mo {
          GHC.Types.I# ds1_s1Mp [Occ=Once!] ->
              case ds1_s1Mp of _ [Occ=Dead] {
                __DEFAULT ->
                    let {
                      sat_s1Ms [Occ=Once] :: GHC.Types.Int
                      [LclId, Str=DmdType] =
                          \u []
                              let {
                                sat_s1Mr [Occ=Once] :: GHC.Types.Int
                                [LclId, Str=DmdType] =
                                    \u [] GHC.Enum.pred GHC.Enum.$fEnumInt wild_s1Mo;
                              } in  f_rn0 sat_s1Mr;
                    } in  GHC.Num.* GHC.Num.$fNumInt wild_s1Mo sat_s1Ms;
                0# -> GHC.Types.I# [1#];
              };
        };
```

Even if I remove all irrelevant information, you will probably not recognize
factorial function, but at least now it looks similar to miniSTG syntax:

```c
f_rn0 = [ds_s1Mn]
        case ds_s1Mn of wild_s1Mo {
          GHC.Types.I# ds1_s1Mp ->
              case ds1_s1Mp of _ {
                __DEFAULT ->
                    let {
                      sat_s1Ms = []
                              let {
                                sat_s1Mr = []
                                    GHC.Enum.pred GHC.Enum.$fEnumInt wild_s1Mo;
                              } in  f_rn0 sat_s1Mr;
                    } in  GHC.Num.* GHC.Num.$fNumInt wild_s1Mo sat_s1Ms;
                0# -> GHC.Types.I# [1#];
              };
        };
```

The biggest difference, except pattern matching on unboxed literals, is `case`
expression:

```c
case a of b {
	__DEFAULT -> ...
	...
}
```

Here `b` is an alias for the result of `a` (note that `a` could be an expression),
it corresponds to "as patter" in haskell. In miniSTG it can be represented as two
nested cases:

```c
case a of {
	b -> case b of {
		...
	}
}
```

Otherwise the conversion is trivial. The patch mentioned at the beginning tries
to automatically convert GHC syntax to miniSTG one.

# Using GHC and miniSTG together

First we need custom prelude that defines all basic declarations (`Prelude.hs`):

```haskell
{-# LANGUAGE PackageImports #-}

module Prelude
( Int
, zero
, one
, plus
, sub
, mul
, eqInt
, Bool (..)
)
where

import "base" Prelude (Bool (..))

data Int = Int

{-# NOINLINE zero #-}
zero :: Int
zero = Int

{-# NOINLINE one #-}
one :: Int
one = Int

{-# NOINLINE plus #-}
plus :: Int -> Int -> Int
plus _ _ = Int

{-# NOINLINE sub #-}
sub :: Int -> Int -> Int
sub _ _ = Int

{-# NOINLINE mul #-}
mul :: Int -> Int -> Int
mul _ _ = Int

{-# NOINLINE eqInt #-}
eqInt :: Int -> Int -> Bool
eqInt _ _ = False
```

Note that implementation for the declarations is not important, we are going to
implement them directly in miniSTG anyway. Now a test module (`Test.hs`):

```haskell
module Test
(test
)
where

pred :: Int -> Int
pred n = n `sub` one

f :: Int -> Int
f n | n `eqInt` zero
    = one
    | True
    = n `mul` f (pred n)

two = plus one one
five = two `plus` two `plus` one

test :: Int
test = f five
```

If you compile this module with `-ddump-ministg`, you get the next:

```c
f = FUN(n_sJm -> case eqIntz1Prelude n_sJm z0eroz1Prelude of {
	 wild_sJn -> case wild_sJn of {
	     Falsez1GHCz1Types -> let {
		  sat_sJp = THUNK(let {
			  sat_sJo = THUNK(subz1Prelude n_sJm onez1Prelude)
			  } in
			  f sat_sJo)
		  } in
		  mulz1Prelude n_sJm sat_sJp;
	     Truez1GHCz1Types -> let {
		 res_var_ = THUNK(onez1Prelude)
		 } in
		 res_var_;
	     };
	 });

two = THUNK(plusz1Prelude onez1Prelude onez1Prelude);

sat_sJr = THUNK(let {
                sat_sJq = THUNK(plusz1Prelude two two)
                } in
                plusz1Prelude sat_sJq onez1Prelude);

testz1Test = THUNK(f sat_sJr);
```

Names are encoded, e.g. `Falsez1GHCz1Types` is a `False` constructor from `GHC.Types` module.
Lets add an entry point:

```c
main = THUNK(testz1Test);
```

and run it:

```bash
$ ministg --noprelude -s EA test.stg
ministg: undefined variable: "eqIntz1Prelude"
```

Oops, we need to define prelude. Create `Prelude.stg` with the next content:

```c
z0eroz1Prelude = CON(Int 0);
onez1Prelude = CON(Int 1);

plusz1Prelude = FUN(x y -> case x of {
	Int i -> case y of {
		Int j -> case plus# i j of {
			k -> let {
				r = CON(Int k)
				} in r;
		};
	};
});

subz1Prelude = FUN(x y -> case x of {
	Int i -> case y of {
		Int j -> case sub# i j of {
			k -> let {
				r = CON(Int k)
				} in r;
		};
	};
});

mulz1Prelude = FUN(x y -> case x of {
	Int i -> case y of {
		Int j -> case mult# i j of {
			k -> let {
				r = CON(Int k)
				} in r;
		};
	};
});

eqIntz1Prelude = FUN(x y -> case x of {
	Int i -> case y of {
		Int j -> case eq# i j of {
			k -> case intToBool# k of {
				True -> true;
				False -> false;
			};
		};
	};
});

true = CON(Truez1GHCz1Types);
false = CON(Falsez1GHCz1Types);
```

Note that here we converted built-in `True` and `False` to the corresponding constructors
from `GHC.Types`. Now we can run the program (note that we removed `--no-prelude` flag):

```bash
$ ministg -s EA test.stg
(Int 120)
```

Yay! We have working frontend for miniSTG! Note that nothing stops us from using advanced
haskell features like type classes because they are compiled out to simple constructs.
For example, lets write the same factorial function using `Eq` type class:

```haskell
class Eq a where
  eq :: a -> a -> Bool

instance Eq Int where
  eq = eqInt

f :: Int -> Int
f n | n `eq` zero
    = one
    | True
    = n `mul` f (pred n)
```

If you don't yet know how type classes work in haskell, then you have a good chance to
figure it out from STG dump!
