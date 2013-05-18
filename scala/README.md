Wisp in scala
=============

This is currently the reference implementation of wisp. 


Build
=====

  * [Download sbt](http://www.scala-sbt.org/download.html) (don't worry, its quite painless)
  * `git clone https://github.com/espringe/wisp.git`
  * `cd wisp/scala`
  * `sbt assembly` (have patience: the combination of the scala compiler, maven repositories and jvm is wrist-cuttingly slow -- go have two or three coffees)

Running
=======

There's a (very simply) launcher script `./wisp`, or a self-contained jar at `target/scala-2.10/wisp.jar`
```
usage: ./wisp file-to-interpret.wisp
````

Interactive Running
===================

The best way is to run from `sbt`. This has the benefit of automatically recompiling, and rerunning on detecting any source changes (in both wisp itself, or in the scala code)

```
sbt
~run ../wisp/test.wisp
```

The way that sbt detects source changes, is because `src/main/resources` is symlinked to `../wisp` -- so if you want to watch a different directory for changes, you'll have to change this sym link.
