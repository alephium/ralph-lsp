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

package org.alephium.ralph.lsp.access.file

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.error.ThrowableError

import java.net.URI
import java.nio.file.{Path, Paths}

object FileAccess {

  // disk file-io
  def disk: FileAccess =
    DiskFileAccess

  def RALPH_LSP_HOME: String =
    ".ralph-lsp"

  def USER_HOME: Option[Path] =
    Option(System.getProperty("user.home"))
      .map(Paths.get(_))

}

/**
 * Implements functions accessing file IO.
 */
trait FileAccess {

  /**
   * Checks if a source-file exists.
   *
   * @param fileURI source-file location
   */
  def exists(
      fileURI: URI,
      index: SourceIndex): Either[CompilerMessage.AnyError, Boolean]

  /**
   * Fetch all workspace source file locations.
   *
   * @param workspaceURI Project/workspace location.
   */
  def list(workspaceURI: URI): Either[CompilerMessage.AnyError, Seq[URI]]

  /**
   * Fetch the source-code of a file.
   *
   * @param fileURI source-code location.
   */
  def read(fileURI: URI): Either[CompilerMessage.AnyError, String]

  /**
   * Fetches the source code of a file, checking if the file exists within a single IO operation.
   *
   * @param fileURI the source code location.
   * @return [[None]] if the file does not exist, else the String content of the file.
   */
  def readIfExists(fileURI: URI): Either[CompilerMessage.AnyError, Option[String]]

  /** Write string to the given file URI. */
  def write(
      fileURI: URI,
      string: String,
      index: SourceIndex): Either[ThrowableError, Path]

}
