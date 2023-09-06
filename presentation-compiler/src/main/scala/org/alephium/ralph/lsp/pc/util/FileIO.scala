package org.alephium.ralph.lsp.pc.util

import org.alephium.ralphc.Compiler

import java.net.URI
import java.nio.file.Path
import scala.io.Source
import scala.util.{Try, Using}

object FileIO {

  def readAllLines(file: URI): Try[String] =
    Using(Source.fromFile(file))(_.mkString)

  def getFiles(path: Path,
               extension: String): Try[Seq[URI]] =
    Try(Compiler.getSourceFiles(path, extension))
      .map(_.map(_.toUri))
}
