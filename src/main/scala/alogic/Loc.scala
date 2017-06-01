package alogic

import scala.collection.concurrent.TrieMap

import scalax.file.Path
import scala.annotation.tailrec

class Loc private (val file: String, val line: Int) {
  override def toString = s"${file}:${line}"
}

object Loc {

  private[this]type LineMap = TrieMap[Range, String]

  // Global object and hence accessed from multiple threads
  private[this] val locMap = TrieMap[String, LineMap]()

  def apply(path: Path, line: Int): Loc = {

    // Map modified location to canonical source location
    @tailrec def loop(file: String, line: Int): (String, Int) = {
      if (!(locMap contains file)) {
        (file, line)
      } else {
        val lineMap = locMap(file)

        // Find the mapping that contains this line
        lineMap.find(_._1 contains line) match {
          // If found a mapping, remap source and location and apply recursively
          case Some((range, source)) => {
            loop(source, line - range.start + 1)
          }
          // If not found a mapping, then adjust our own line number based on
          // how many lines were added by the mappings preceding this line
          case None => {
            val addedLines = (0 /: lineMap.keys.filter(_.end < line))(_ + _.size)
            (file, line - addedLines)
          }
        }
      }
    }

    val (f, l) = loop(canon(path), line)

    new Loc(f, l)
  }

  def apply(path: String, line: Int): Loc = apply(Path.fromString(path), line)

  // Adjust map so that lines form 'file' that are in 'range'
  // will be printed as belonging to file 'source'
  def remap(path: Path, range: Range, source: Path) = {
    val lineMap = locMap.getOrElseUpdate(canon(path), TrieMap[Range, String]())
    lineMap(range) = canon(source)
  }

  // Canonicalise file names so equivalent paths map to the same string
  private[this] def canon(path: Path): String = path.toRealPath().path

}
