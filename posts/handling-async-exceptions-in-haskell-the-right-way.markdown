---
title: Handling (async) exceptions in haskell: the right way
published: 2014-11-16
---

(Originally posted [here](https://github.com/Yuras/io-region/wiki/Handling-%28async%29-exceptions-in-haskell:-the-right-way))

(I realize that I'm not authoritative enough to claim that it is
*the right way*. But we don't have any authoritative claim anyway,
so I'm free to make an attempt.)

We'll mostly discuss exception handling in cleanup actions. You should already
understand basics of exception handling: synchronous exception, asynchronous
exceptions, interruptible actions, masking asynchronous exceptions. You probably
should already understand *why* exception handling is important.

## Exception safety

The goal of exception handling is to provide some guarantees that, in case of
failure, the user of your code can rely on. The code is exception-safe if it
provides reasonable guarantees.

What guarantees are reasonable, actually depends on particular case. Informally,
it should be possible at least to continue executing unrelated tasks.
For example, if database driver in your web application failed at some point,
you still should be able to respond with error page; but if the driver failed
to free memory, then it is probably unreasonable, because you'll run out of
memory at some point. We'll say that the code is exception-safe if it provides
such reasonable guarantees.

There is a continuum of reasonable guarantees, but it is convenient to say
about *levels* of exception safety. It is common to use [four levels][1]:

 - No-throw: the component will handle all failures itself
 - Strong: in case of failure, any side effect will be reverted
 - Basic: in case of failure, any resources will be freed and invariants of the
   component preserved, so you can continue using the component.
 - No safety: you should terminate the application ASAP to prevent further
   damage.

## Optimal level of safety

It is not possible to achieve no-throw or even strong exception safety in all
cases, but basic level is the must-have except probably some special cases.

For reusable components you should try to achieve the highest possible level.
For example, if you can't revert all side effects, then you should revert as
much as it is possible with reasonable efforts. Usually a number of trade offs
are involved, so you should decide what level of safety to provide in each
case separately.

## General approach

Most of errors in exception handling come not from failure to handle
exceptional situation, but from failure to recognize it's possibility. The average
developer tries to handle exceptions he is aware of, or even when they occur in
production. Obviously it is not possible to provide any guarantees with such
the approach. Instead we should assume that any component, we are using, can
fail. Then we ask ourselves, what exception safety guarantees it provides, and
decide how to handle this case.

It is almost impossible to achieve even basic safety level if you are using at
least one component with lower level, so all reusable components should provide
at least basic guarantees.

See also [here][3] for more details on exception safety analysis without async
exceptions (C++)

## Asynchronous exceptions

Async exceptions, unlike synchronous, can be raised at any moment. That makes
reasoning about them really hard. But for purposes of exception handling it is
mostly irrelevant because in most cases they are masked, so they behave like
synchronous exceptions. Obviously they add complexity to exception handling
though.

The source of the complexity is the next: there are components, that have
different exception-safety guarantees for sync and async exceptions. Please
stop here, take a cup of beer and think about it, because it is the most
important thing you should know about async exception. That is the *only*
source of complexity.

So in the worse case async exceptions double complexity of exception handling,
because for each component you need to think about two levels of complexity.
I personally find that it is pretty reasonable price. Though in reality the
additional complexity is small in most cases because:
 - most components have the same safety level both for sync and async
   exceptions
 - it is usually possible to rely only on intersection of sync and async
   safety guarantees
 - as the last resort, you can disable async exceptions using
   uninterruptibleMask

## Reality

OK, in theory async exceptions are not so hard. But people find it hard
regardless any theoretical arguments. Probably the theory is wrong?

As part of standardization process, David Abrahams produced an exception-safe
reference implementation of (part of) C++ standard library, STL. To test
exception safety, he instrumented the code to make it possible to throw
exception at almost any possible point of failure (see [here][2], section 7). Do you
see the point already? He introduced async exception in order to test handling
of sync exceptions!

Handling exceptions in presence of asynchronous exceptions looks hard because they
uncover most of errors in exception handling. There is nothing complex in async
exceptions per se, but exception handling is complex itself, and async
exceptions just force us to fix bugs. And that is excellent, isn't it?

Haskell has embedded tool to test exception handling, we call it "asynchronous
exceptions". That potentially makes haskell the best language ever. Lets
learn how to use the tool instead of ignoring it. 

[Discussion](http://www.reddit.com/r/haskell/comments/2mfxka/handling_async_exceptions_in_haskell_the_right_way/)


 [1]: http://en.wikipedia.org/wiki/Exception_safety

 [2]: http://www.boost.org/community/exception_safety.html

 [3]: http://erdani.com/publications/cuj-2003-12.pdf
