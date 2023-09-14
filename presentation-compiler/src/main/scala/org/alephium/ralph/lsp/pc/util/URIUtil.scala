package org.alephium.ralph.lsp.pc.util

import java.net.URI
import java.nio.file.Paths

object URIUtil {

  def getFileName(uri: URI): String =
    Paths.get(uri).toFile.getName

  // TODO: Probaly a easier way to do this using URI.
  def getFileExtension(uri: URI): String =
    Paths.get(uri).toFile.getName.dropWhile(_ != '.').drop(1)

}
