// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.utils

import java.net.URI
import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters.IteratorHasAsScala

object URIUtil {

  /**
   * Build URI and clean it from escaped characters. Happen on Windows
   */
  def uri(string: String): URI =
    if (scala.util.Properties.isWin) {
      new URI(java.net.URLDecoder.decode(string, "UTF-8"))
    } else {
      new URI(string)
    }

  /**
   * Checks if the scheme component of this URI is `file`.
   */
  def isFileScheme(uri: URI): Boolean =
    uri.getScheme == "file"

  def getFileName(uri: URI): String =
    Paths.get(uri).toFile.getName

  // TODO: Probably an easier way to do this using URI.
  def getFileExtension(uri: URI): String =
    getFileName(uri).dropWhile(_ != '.').drop(1)

  /** Is the child [[URI]] within the parent [[URI]] */
  def contains(
      parent: URI,
      child: URI): Boolean =
    contains(
      parent = Paths.get(parent),
      child = Paths.get(child)
    )

  /** Is the child [[Path]] within the parent [[URI]] */
  def contains(
      parent: URI,
      child: Path): Boolean =
    contains(
      parent = Paths.get(parent),
      child = child
    )

  /** Is the child [[URI]] within the parent [[Path]] */
  def contains(
      parent: Path,
      child: URI): Boolean =
    contains(
      parent = parent,
      child = Paths.get(child)
    )

  /** Is the child [[Path]] within the parent [[Path]] */
  def contains(
      parent: Path,
      child: Path): Boolean =
    parent
      .resolve(child)
      .startsWith(parent)

  def isFileName(
      fileURI: URI,
      fileName: String): Boolean =
    getFileName(fileURI) == fileName

  def isFirstChild(
      parent: URI,
      child: URI): Boolean =
    isFirstChild(
      parent = Paths.get(parent),
      child = Paths.get(child)
    )

  def isFirstChild(
      parent: Path,
      child: Path): Boolean =
    parent.resolve(child).getParent == parent

  /** Similar to [[List.takeRight]] fetch the last `n` nested paths */
  def takeRight(
      uri: URI,
      count: Int): Option[Path] = {
    val right =
      Paths
        .get(uri)
        .iterator()
        .asScala
        .toList
        .takeRight(count)
        .map(_.toString)

    right.headOption map {
      head =>
        Paths.get(head, right.tail: _*)
    }
  }

  /**
   * Drops the specified number of path segments from the tail end of the URI path.
   *
   * Prerequisite: An empty URI cannot be created so the drop count must be less than segment count.
   *
   * @param uri   The original URI.
   * @param count The number of path segments to drop the tail end.
   * @return A new URI with tail segments removed.
   */
  def dropRight(
      uri: URI,
      count: Int): URI = {
    val segments =
      uri
        .getPath
        .split("/")
        .filter(_.nonEmpty)

    // We do not have a use case where we need to drop more than the segment count.
    // If such case does occur, that is a bug and should be resolved with the following condition check.
    require(count < segments.length, s"Drop count of $count is not less than segment count of ${segments.length}")

    val trimmedPath =
      segments
        .dropRight(count)
        .mkString("/", "/", "")

    new URI(
      uri.getScheme,
      uri.getUserInfo,
      uri.getHost,
      uri.getPort,
      trimmedPath,
      uri.getQuery,
      uri.getFragment
    )
  }

  /**
   * String literal that defines an import statement for a source file.
   *
   * @return If the file is named `std/my_code.ral`, the import statement returned
   *         is `import "std/my_code"`.
   */
  def importIdentifier(uri: URI): Option[String] =
    takeRight(
      uri = uri,
      count = 2
    ) map {
      identifier =>
        val string =
          identifier
            .iterator()
            .asScala
            .mkString("/") // import statements use forward slash.

        string.substring(0, string.lastIndexOf("."))
    }

}
