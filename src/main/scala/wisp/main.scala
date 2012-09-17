package wisp

import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.StandardWatchEventKinds
import java.text.DecimalFormat

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
      val path = Paths.get(rest.head)

      val dag = loadAll(Dag[Path, Any](), path)

      if (verbose) {
        println("File dependency graph:\n" + dag.toAscii)
      }

      if (watch)
        runWatch(dag, verbose)
      else
        runDag(dag, verbose)

    }
  }

  def loadAll(current: Dag[Path, Any], path: Path): Dag[Path, Any] = {

    val (imports, value) = Reader(path)

    imports.foldLeft(current.add(path, value, imports)) {
      (a, b) =>
        if (!a.payload.contains(b))
          loadAll(a, b)
        else
          a
    }

  }

  def runDag(dag: Dag[Path, Any], verbose: Boolean): Any = {

    val data = dag.topologicalSort.reverse.map(x => x -> dag.payload(x))

    val r = data.foldLeft(Interpretter.startingEnv: Any) {
      case (env, (path, form)) =>
        require(env.isInstanceOf[Dict], "Expected the result of an import to give us an environment, instead found: " + env)

        if (verbose)
          println("About to interpret file: " + path)

        val (result, t) = timeFunc(Interpretter(form,env.asInstanceOf[Dict]))

        if (verbose)
          println("..took " + t)
          
        result
    }

    println(r)
  }

  def runWatch(dag: Dag[Path, Any], verbose: Boolean) {
    sys.error("Run Watch not yet supported")
  }

  def blockOn(watching: IndexedSeq[Path]): Unit = {

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

      for (event <- wk.pollEvents()) {

        val context = watchable.resolve(event.context().asInstanceOf[Path])

        if (check.contains(context.toString())) {
          return
        }

      }
      wk.reset()
    }

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

