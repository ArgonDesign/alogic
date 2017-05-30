package alogic

import scala.collection.concurrent.TrieMap

import scalax.file.Path

object LocMap {

  type LineMap = TrieMap[Range, String]

  // Global object and hence accessed from multiple threads
  val locMap = TrieMap[String, LineMap]()

  // Canonicalise file names so equivalent paths map to the same string
  private def canon(path: Path): String = path.toRealPath().path

  private def canon(path: String): String = Path.fromString(path).toRealPath().path

  // Adjust map so that lines form 'file' that are in 'range'
  // will be printed as belonging to file 'source'
  def remap(file: Path, range: Range, source: Path) = {
    val lineMap = locMap.getOrElseUpdate(canon(file), TrieMap[Range, String]())
    lineMap(range) = canon(source)
  }

  // Map modified location to canonical source location
  def apply(loc: Loc): Loc = {
    val file = canon(loc.file)
    if (!(locMap contains file)) {
      loc
    } else {
      val lineMap = locMap(file)

      // Find the mapping that contains this line
      val mapping: Option[(Range, String)] = lineMap.find(_._1 contains loc.line)

      mapping match {
        // If found a mapping, remap source and location and apply recursively
        case Some((range, source)) => {
          apply(Loc(source, loc.line - range.start + 1))
        }
        // If not found a mapping, then adjust our own line number based on
        // how many lines were added by the mappings preceding this line
        case None => {
          val addedLines = (0 /: lineMap.keys.filter(_.end < loc.line))(_ + _.size)
          Loc(loc.file, loc.line - addedLines)
        }
      }
    }
  }
}

case class Loc(file: String, line: Int) {
  override def toString = {
    val rloc = LocMap(this)
    val absPath = Path.fromString(rloc.file).toRealPath().path
    s"${absPath}:${rloc.line}"
  }
}
