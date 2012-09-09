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

Wisp is currently written in Scala, which unfortunately is on the JVM. Eventually wisp will be hosted ontop of javascript, but in the mean time you will need `sbt` (scala build tool) to build, which will also handle getting the dependencies (inclusive of scala).

  * [Download sbt](http://www.scala-sbt.org/download.html) (don't worry, its quite painless)
  * `git clone https://github.com/espringe/wisp.git`
  * `cd wisp`
  * `sbt assembly` (have patience: the combination of the scala compiler, maven repositories and jvm is wrist-cuttingly slow)


Running
=======

Either run the `./wisp` script, or if you prefer there is a self-contained jar at `target/wisp.jar`

```
usage: wisp [-i | -w] file-to-interpret.wisp

Valid options are:
         
  -i        Interactive. After running the program, drop into
            an interactive repl session
          
          OR

  -w        Watch. After running the program, watch the file
            and its transitive dependencies for any changes. If
            any files change, rerun the program
```

Both options are extremely useful for wisp development


Documentation
====

Barely, but look in the `docs/` directory

