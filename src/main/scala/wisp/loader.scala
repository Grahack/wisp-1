package wisp

import java.nio.file.Paths
import java.nio.file.Path

object Loader {

  def apply(path: Path, verbose: Boolean) = loadAll(Dag[Path, Any](), path, verbose)

  def loadAll(current: Dag[Path, Any], path: Path, verbose: Boolean): Dag[Path, Any] = {
    if (verbose)
      println("Loading file: " + path)

    def includeToString(inc: Any): String = { // inc should be W but scala's 2.9.x pattern match isn't smart enough
      require(inc.isInstanceOf[WVect], "Imports must take a literal string.., found: " + inc)

      val charVect = inc.asInstanceOf[WVect].value.map {
        x =>
          require(x.isInstanceOf[WChar], "Expected a char, found: " + x)
          x.asInstanceOf[WChar].value
      }
      val sb = new scala.collection.mutable.StringBuilder(charVect.length)
      charVect.foreach { sb += _ }
      sb.result
    }

    val res = Reader(path) match {
      case Seq(Seq('include, importPaths @ _*), rest @ _*) =>
        (importPaths.map(x => path.resolveSibling(includeToString(x))) -> rest)
      case rest => (Seq() -> rest)
    }

    //    val importPaths = imports.map(path.resolveSibling(_)).toSet // TODO: check for dupes ?
    //
    //    importPaths.foldLeft(current.add(path, value, importPaths)) {
    //      (a, b) =>
    //        if (!a.payload.contains(b))
    //          loadAll(a, b, verbose)
    //        else
    //          a
    //    }

    null

  }

}