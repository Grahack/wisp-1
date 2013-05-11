Wisp
----

Wisp is a whitespace lisp. It is designed to be extremely powerful and flexible, more so than any other programming language. More than just a programming language, it's more of a programming language language.

It can effectively be thought of a programming language with a minimal syntax, an untyped and functional core, a huge amount of runtime reification (i.e. enough for being able to print very helpful compile time, or runtime errors), powerful abstractions that blur the line between compile-time and runtime, macros and functions.

Wisp is hugely suited for tasks like making embedded DSLs and abstractions.

For instance, imagine an SQL abstraction layer. The normal approach would be to effectively create a mini-language or framework (with clever use of objects, functions and what not) that you could combine in some fashion to build a query object. That query object would effectively be interpretted into an SQL statement. While this works, to make it bareable it tends to have to use such advanced language features that only an expert in the host language can use it (defeating a large amount of the benefits of a DSL) as even things like errors are reported at the host language-level. Not to mention host-language limitations can be absolutely crippling -- like you might want to be able to accept an arbitrary function for the `where` clause -- but without reinventing an entire language yourself, or using brittle and ridiciliously complicated compile-time stuff -- its impossible in a mainstream language to inspect the function and decide if you can build a SQL where clause out of it (and if not, perhaps do it as a client-side filter, or give a *readable* error).

In wisp, functions are not opaque (you can introspect them, with source and dynamic information) and do a huge amount of cool things


Interesting features:

  * Significant whitespace
  * [First-class combinators](https://secure.wikimedia.org/wikipedia/en/wiki/Fexpr)
  * Static Scoping with forward references (like you'd expect from a modern language)
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


Building
========

The most complete implementation of wisp is written in Scala, so if you want to try wisp -- that's definitely the way to go. The goal will be to self-host, via use of an internal library. However, this has proven to be a dauntingly large task, and won't happen immediately. In the mean time, Scala has unfortunately proven to be largely unsuitable for writing an interpretter (alhtough, it has completely ridiculous startup times, slow, and a total pain in the ass to "deploy" to anyone fortunate enough to not have the jvm on their system. 

  * [Download sbt](http://www.scala-sbt.org/download.html) (don't worry, its quite painless)
  * `git clone https://github.com/espringe/wisp.git`
  * `cd wisp/scala`
  * `sbt assembly` (have patience: the combination of the scala compiler, maven repositories and jvm is wrist-cuttingly slow -- go have two or three coffees)


Running
=======

There's a launcher script `./scala/wisp <file.wisp>`, or a self-contained jar at `target/wisp.jar`, or if you don't want to pay for jvm startup each time, you can launch `sbt` and from within the console type: `run <file.wisp>` (which will run, and return you to the sbt console)

```
usage: wisp file-to-interpret.wisp
````

Haskell
=======

There's an initial haskell version, which is ridiciliously fast (x100 faster than scala). While it'd be great if this was maintained, at the moment the priority is on the bootstrap and scala version.

Documentation
====

Barely, but look in the `docs/` directory




