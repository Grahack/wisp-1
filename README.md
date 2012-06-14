Wisp
----

Wisp is a whitespace lisp. It is designed to be extremely powerful and flexible, more so than any other programming language. Interesting features:

  * Significant whitespace
  * [First-class combinators](https://secure.wikimedia.org/wikipedia/en/wiki/Fexpr)
  * Immutable, first class environment (allows static scoping, dynamic scoping, rolling back changes)

Status
======
A toy, and I won't claim otherwise until it's self-hosting.


Building
========

Wisp is currently written in Scala, which unfortunately is on the JVM. Eventually wisp will be hosted ontop of llvm and/or javascript, but in the mean time you will need `sbt` (scala build tool) to build, which will also handle getting the dependencies (inclusive of scala).

  * [Download sbt](http://www.scala-sbt.org/download.html) (don't worry, its quite painless)
  * `git clone https://github.com/espringe/wisp.git`
  * `cd wisp`
  * `sbt assembly` (have patience: the combination of the scala compiler, maven repositories and jvm is wrist-cuttingly slow)


Running
=======

Either run the `./wisp` script, or if you prefer there is a self-contained jar at `target/wisp.jar`


Documentation
====

Barely, but look in the `docs/` directory

