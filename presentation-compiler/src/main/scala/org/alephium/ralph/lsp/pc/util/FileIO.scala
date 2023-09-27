package org.alephium.ralph.lsp.pc.util

import java.net.URI
import java.nio.file.{Files, Path}
import scala.io.Source
import scala.util.{Try, Using}

object FileIO {

  def readAllLines(file: URI): Try[String] =
    Using(Source.fromFile(file))(_.mkString)

  def exists(file: Path): Try[Boolean] =
    Try(Files.exists(file))
}
