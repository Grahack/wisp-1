Wisp
----

Wisp is a whitespace lisp. It's designed to be simple, yet extremely powerful and flexible. Much more so than any other programming language out there, and more than just a programming language -- its a programming language language.

Wisp is best thought of as having a minimal and consistent syntax -- with an untyped and functional core. It has huge amount of runtime reification (e.g. enough for being able to print very helpful compile time, or runtime errors), powerful abstractions that blur the line between compile-time and runtime, macros and functions.

Relative to other lisps, wisp has more datatypes. The function core includes `Symbol`, `List`, `FuncCall` (function application, which breaking lisp tradition is not a list), `Char`, `Bool` and a `Dict`). And by default is lazy, which makes building composible functions and dealing with IO and streams fun.

Wisp is hugely suited for tasks like making embedded DSLs and abstractions.

For instance, imagine an SQL abstraction layer. The normal approach would be to effectively create a mini-language or framework (with clever use of objects, functions and what not) that you could combine in some fashion to build a query object. That query object would effectively be interpretted into an SQL statement. While this works, to make it bareable it tends to have to use such advanced language features that only an expert in the host language can use it (defeating a large amount of the benefits of a DSL) as even things like errors are reported at the host language-level. Not to mention host-language limitations can be absolutely crippling -- like you might want to be able to accept an arbitrary function for the `where` clause -- but without reinventing an entire language yourself, or using brittle and ridiciliously complicated compile-time stuff -- its impossible in a mainstream language to inspect the function and decide if you can build a SQL where clause out of it (and if not, perhaps do it as a client-side filter, or give a *readable* error).

In wisp, functions are not opaque (you can introspect them, with source and dynamic information) and do a huge amount of cool things


Interesting features:

  * Significant whitespace
  * [First-class combinators](https://secure.wikimedia.org/wikipedia/en/wiki/Fexpr)
  * Purely functional core (or close to it)
  * Compile to efficient javascript and llvm (done as a library! Not as a compiler)

Status
======

  * Language fundamentals **Done**
  * Simple interpretter **Done**
  * Core library **In progress**
  * Self-hosting **TODO**
  * Javascript compiler **TODO**
  * Documentation **TODO**
  * Static analysis **TODO**

Scala
=====

The most complete implementation of wisp is written in Scala, and currently the reference implementation. If you want to use wisp, the scala implementation is the one you should be using. The goal is to self host, via the use internal library. However, this has proven to be a dauntingly large task, and won't happen immediately. In the mean time, scala will have to do.

For more information about the scala implemention, please see `scala/README.md`


Haskell
=======

There's an initial haskell version `haskell`, which is ridiciliously fast (x100 faster than scala). It's got quite out of date, if anyone wants to pick up where I left off, and get it in-line with the scala implemenation -- it would be hugely welcome


C++
===

See `cpp` for a *very* out of date version. Again, if anyone wants to pick up the torch, all pull requests would be well recieved. Be forewarned, C++ is a tricky language in itself -- implementing the features wisp requires (laziness, structural sharing, tail calls, etc.) is challenging, as one can not simply hoist onto the language itself.


Wisp
====

This is the where the interesting work should happen. Wisp in `wisp`. A standard library will need to be constructed, and a compile built. The first target language will be javascript. Here is where the the canonical test should live, for both testing implementations of wisp. And testing wisp in wisp.


Other
=====

If anyone has desires, any other language implementation would be well recieved.






Documentation
=============

Barely, but look in the `docs/` directory
