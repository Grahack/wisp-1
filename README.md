Wisp
----

Wisp is a whitespace lisp. It is designed to be extremely powerful and flexible, more so than any other programming language. Interesting features:

  * Significant whitespace
  * [First-class combinators](https://secure.wikimedia.org/wikipedia/en/wiki/Fexpr)
  * Static Scoping with forward references (like you'd expect from a modern language)
  * Purely functional (or close to it)
  * Compile to efficient javascript
  * Based on persistent vectors and maps (not linked-lists like most lisps)

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

The most complete implementation of wisp is written in Scala, so if you want to try wisp -- that's definitely the way to go. The goal will be to self-host, but this has proven to be a dauntingly large task, and won't happen immediately. In the mean time, Scala has unfortunately proven to be largely unsuitable for writing an interpretter (completely ridiculous startup times, slow, and a total pain in the ass to "deploy" to anyone fortunate enough to not have the jvm on their system. So I'm looking into using rewriting it in haskell. In the mean time, you'll have to use the scala version:

  * [Download sbt](http://www.scala-sbt.org/download.html) (don't worry, its quite painless)
  * `git clone https://github.com/espringe/wisp.git`
  * `cd wisp/scala`
  * `sbt assembly` (have patience: the combination of the scala compiler, maven repositories and jvm is wrist-cuttingly slow -- go have two or three coffees)


Running
=======

There's a launcher script `./scala/wisp <file.wisp>`, or a self-contained jar at `target/wisp.jar`, or if you don't want to pay for jvm startup each time, you can launch `sbt` and from within the console type: `run <file.wisp>` (which will run, and return you to the sbt console)

```
usage: wisp file-to-interpret.wisp


Documentation
====

Barely, but look in the `docs/` directory




