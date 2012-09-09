package wisp

import java.nio.file.Paths
import java.nio.file.Path
import jline.console.ConsoleReader
import jline.console.completer.Completer
import java.nio.file.StandardWatchEventKinds

object Main {
  def main(args: Array[String]) {

    val interactive = args.contains("-i")
    val watch = args.contains("-w")

    require(!(interactive && watch), "Can't watch the filesystem and use interactive as well")

    val rest = args.filter(x => x != "-i" && x != "-w")

    if (rest.size != 1) {
      println(
        """Welcome to Wisp!
          
usage: wisp [-i | -w] file-to-interpret.wisp

Valid options are:
         
  -i        Interactive. After running the program, drop into
            an interactive repl session
          
          OR

  -w        Watch. After running the program, watch the file
            and its transitive dependencies for any changes. If
            any files change, rerun the program""")
    } else {
      val path = Paths.get(rest.head)

      if (interactive)
        runInteractive(path)
      else if (watch)
        runWatch(path)
      else
        println(Interpretter(path)._1)

    }
  }

  def runWatch(path: Path) = {

    var watching = IndexedSeq(path)

    while (true) {
      try {
        val ((res, _, files), time) = timeFunc(Interpretter(path))

        println("Res: " + res)
        println("Took: " + time + "ms")

        watching = files
      } catch { case x: Throwable => println("Caught errror: " + x) }

      require(watching.nonEmpty)

      print("Waiting on file changes...")
      blockOn(watching)
      println("\n")
    }

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

  def runInteractive(path: Path) {

    var (res, env, _) = Interpretter(path)

    val console = new ConsoleReader

    env = env + (Symbol(":res0") -> res)
    console.println(":res0 = " + res)

    console.addCompleter(new WispCompleter(env))

    var count = 0

    while (true) {
      val line = console.readLine("~> ")

      if (line == null)
        return

      try {
        val processed = Reader(line).foreach { processed =>

          val (res, time) = timeFunc(Interpretter.eval(env, processed))

          count = count + 1
          val newSymb = ":res" + count
          env = env + (Symbol(newSymb) -> res)

          val summary = newSymb + " = " + res
          val info = "[Took " + time + "ms]"

          val spaces = console.getTerminal.getWidth - summary.length - info.length

          console.print(summary)
          if (spaces > 0)
            console.print(" " * spaces)
          else
            console.print("\n")
          console.println(info)
          console.flush()
        }

      } // catch { case x => console.println("Caught errror: " + x) }
    }
  }

  def timeFunc[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    (ret, (System.nanoTime - s) / 1e6)
  }
}

class WispCompleter(env: Dict) extends Completer {

  def complete(buffer: String, at: Int, results: java.util.List[CharSequence]) = {

    val (before, start) = split(buffer, at)

    env.foreach(f => {
      // CHECK: the static-env really should never contain non-symbols, eh?
      val n = f._1.asInstanceOf[Symbol].name
      if (n.startsWith(before))
        results.add(n + " ")
    })

    start
  }

  def split(buffer: String, at: Int): (String, Int) = {
    val from = math.max(at - 1, 0)

    // TODO: probably a nicer way to write this then nested maxes
    val lio = math.max(buffer.lastIndexOf(' ', from),
      math.max(buffer.lastIndexOf('(', from),
        buffer.lastIndexOf(')', from)))
    val start = if (lio < at) lio + 1 else at
    val before = buffer.substring(math.min(start, buffer.length), at)

    (before, start)
  }
}
