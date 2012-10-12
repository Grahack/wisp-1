package wisp

import java.nio.file.Paths
import java.nio.file.Path

object Loader {

  def apply(path: Path, verbose: Boolean) = loadAll(Dag[Path, Any](), path, verbose)

  def loadAll(current: Dag[Path, Any], path: Path, verbose: Boolean): Dag[Path, Any] = {
    if (verbose)
      println("Loading file: " + path)

    
    def includeToString(inc: W): String = {
      
      null
    }

    val res = Reader(path) match {
      case Seq(Seq('include, importPaths @ _* ), rest @ _* ) =>
        (importPaths.map(x => path.resolveSibling(x.toString)) -> rest)
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