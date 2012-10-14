package wisp

import java.nio.file.Paths
import java.nio.file.Path

object Loader {

  def apply(file: String, verbose: Boolean) = loadAll(Dag[Path, Seq[W]](), Paths.get(file), verbose)

  def loadAll(current: Dag[Path, Seq[W]], path: Path, verbose: Boolean): Dag[Path, Seq[W]] = {
    if (verbose)
      println("Loading file: " + path)

    def includeToString(inc: Any): String = { // inc should be W but scala's 2.9.x pattern match isn't smart enough
      require(inc.isInstanceOf[Vect], "Imports must take a literal string.., found: " + inc)

      val charVect = inc.asInstanceOf[Vect].value.map {
        x =>
          require(x.isInstanceOf[WChar], "Expected a char, found: " + x)
          x.asInstanceOf[WChar].value
      }
      val sb = new scala.collection.mutable.StringBuilder(charVect.length)
      charVect.foreach { sb += _ }
      sb.result
    }

    val readResults = Reader(path)

    require(readResults.nonEmpty, "A file must have at least one form: " + path)

    // holy crap this is ugly, we really need working patten matching..
    val (includePaths, topLevels) = {
      val firstForm = readResults.head
      if (firstForm.isInstanceOf[WList]) {
        val firstList = firstForm.asInstanceOf[WList].value
        firstList.headOption.flatMap {
          pos =>
            if (pos == 'include)
              Some(firstList.tail.map(includeToString(_)).map(path.resolveSibling(_)))
            else None
        }.map(x => x -> readResults.tail).getOrElse(Seq(), readResults)
      } else
        Seq() -> readResults
    }

    includePaths.foldLeft(current.add(path, topLevels, includePaths.toSet)) {
      (a, b) =>
        if (!a.payload.contains(b))
          loadAll(a, b, verbose)
        else
          a
    }
  }


}