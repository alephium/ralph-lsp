package org.alephium.ralph.lsp.pc.util

import java.net.URI
import java.nio.file.{Path, Paths}

object URIUtil {

  def getFileName(uri: URI): String =
    Paths.get(uri).toFile.getName

  // TODO: Probably an easier way to do this using URI.
  def getFileExtension(uri: URI): String =
    Paths.get(uri).toFile.getName.dropWhile(_ != '.').drop(1)

  /** Is the child [[URI]] within the parent [[URI]] */
  def isChild(parent: URI,
              child: URI): Boolean =
    isChild(
      parent = Paths.get(parent),
      child = Paths.get(child)
    )

  /** Is the child [[Path]] within the parent [[Path]] */
  def isChild(parent: Path,
              child: Path): Boolean =
    parent
      .resolve(child)
      .startsWith(parent)

}
