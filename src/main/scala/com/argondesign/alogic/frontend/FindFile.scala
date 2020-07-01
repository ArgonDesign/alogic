package com.argondesign.alogic.frontend

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import java.util.function.BiPredicate

import scala.jdk.CollectionConverters._

object FindFile {

  def apply(tail: String, searchPaths: List[File], maxDepth: Int = Int.MaxValue): Option[File] = {
    val paths = searchPaths.to(LazyList) filter { _.exists } map { _.toPath }

    val predicate = new BiPredicate[Path, BasicFileAttributes] {
      override def test(path: Path, attr: BasicFileAttributes): Boolean = path endsWith tail
    }

    val results = paths flatMap { Files.find(_, maxDepth, predicate).iterator.asScala }

    results.headOption map { _.toFile.getCanonicalFile }
  }

}
