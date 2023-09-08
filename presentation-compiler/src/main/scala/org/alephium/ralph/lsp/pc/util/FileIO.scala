package org.alephium.ralph.lsp.pc.util

import java.net.URI
import scala.io.Source
import scala.util.{Try, Using}

object FileIO {

  def readAllLines(file: URI): Try[String] =
    Using(Source.fromFile(file))(_.mkString)
}
