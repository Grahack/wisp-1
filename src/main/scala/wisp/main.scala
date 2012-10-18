package wisp

import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.StandardWatchEventKinds
import java.text.DecimalFormat

import scala.collection.GenTraversable
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

object Main {

  def main(args: Array[String]) {

    val watch = args.contains("-w")
    val verbose = args.contains("-v")

    val rest = args.filter(x => x != "-w" && x != "-v")

    if (rest.size != 1) {
      println(
        """Welcome to Wisp!
          
usage: wisp [-w -d] file-to-interpret.wisp

Valid options are:

  -w        Watch. After running the program, watch the file
            and its transitive dependencies for any changes. If
            any files change, rerun the program

  -v        Verbose. Output some verbose information, such as
            awesome ascii-art dependency graphs. """)
    } else {

      val dag = Loader(rest.head, verbose)

      if (verbose)
        println("File dependency graph:\n" + dag.toAscii)

      if (watch)
        runWatch(dag, verbose)
      else
        runDag(dag)

    }
  }

  def runDag(dag: Dag[Path, Seq[W]]) = {

    val data: Seq[W] = dag.topologicalSort.reverse.map(x => dag.payload(x)).flatten

    val (res, time) = timeFunc(data.foldLeft(new Dict(scala.collection.immutable.HashMap()): Any) {
      case (env, form) =>
        require(env.isInstanceOf[Dict], "Expected the result of the non-last statement to give us an environment, instead found: " + env)

        Interpretter.eval(env.asInstanceOf[Dict].value, form)
    })
    
    println("Took time: " + time + " result: " + res)

  }

  def runWatch(dag: Dag[Path, Seq[W]], verbose: Boolean) {
    val entry = dag.root

    var d = dag

    while (true) {
      try {
        //   runDag(d, verbose)
      } catch {
        case x: Throwable => println("Caught exception: " + x)
      }

      println("\nWaiting on file changes...")
      val changedPaths = blockOn(dag.payload.keys)

      if (verbose)
        println("Detected changed: " + changedPaths.mkString(", "))

      val ancestors = for (p <- changedPaths) yield d.ancestors(p)
      val all = ancestors.reduce(_ ++ _)

      for (p <- all) {
        d = d.remove(p)
      }

      // d = loadAll(d, entry, verbose)
    }
  }

  def blockOn(watching: Iterable[Path]): Iterable[Path] = {

    import java.nio.file.StandardWatchEventKinds._
    import java.nio.file.WatchEvent;
    import java.nio.file.WatchKey;
    import java.nio.file.WatchService;
    import java.nio.file.Files
    import java.nio.file.FileSystems

    val fs = FileSystems.getDefault // possible to not be on default fs?

    val check = watching.map(_.toString()).toSet

    val watcher = fs.newWatchService()

    watching.foreach { p =>
      assert(p.getFileSystem() == fs)
      p.getParent().register(watcher, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE)
    }

    import scala.collection.JavaConversions._
    while (true) {
      val wk = watcher.take()

      val watchable = wk.watchable().asInstanceOf[Path]

      val changed = for (context <- wk.pollEvents().map(event => watchable.resolve(event.context().asInstanceOf[Path])) if check.contains(context.toString()))
        yield context

      if (changed.nonEmpty)
        return changed.toList

      wk.reset()
    }

    null // please the type checker
  }

  def timeFunc[A](fn: => A) = {
    val s = System.nanoTime
    val ret = fn
    val f = System.nanoTime

    val d = System.nanoTime - s

    val format = new DecimalFormat("#.##")

    val time = (f - s) match {
      case ns if ns < 1000 => ns + "ns"
      case µs if µs < 1e6 => format.format(µs / 1e3) + "µs"
      case ms if ms < 1e9 => format.format(ms / 1e6) + "ms"
      case s => format.format(s / 1e9) + "s"
    }

    (ret, time)
  }
}

