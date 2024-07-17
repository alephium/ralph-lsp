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
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.error._
import org.alephium.ralph.lsp.access.util.TryUtil
import org.alephium.ralphc.{Compiler => RalphC}

import java.io.FileNotFoundException
import java.net.URI
import java.nio.file.{Path, Paths, Files}
import scala.io.Source
import scala.util.{Using, Success, Failure}

/**
 * Implements functions accessing on-disk file IO.
 *
 * @note Access to this object is private.
 *       PresentationCompiler does not directly accesses this code.
 */

private object DiskFileAccess extends FileAccess {

  /** @inheritdoc */
  override def exists(
      fileURI: URI,
      index: SourceIndex): Either[ThrowableError, Boolean] =
    try
      Right(Files.exists(Paths.get(fileURI)))
    catch {
      case throwable: Throwable =>
        val error =
          ThrowableError(
            title = s"Failed to check if file '$fileURI' exists",
            throwable = throwable,
            index = index
          )

        Left(error)
    }

  /** @inheritdoc */
  def list(workspaceURI: URI): Either[CompilerMessage.AnyError, Seq[URI]] =
    try {
      val uris =
        RalphC
          .getSourceFiles(
            path = Paths.get(workspaceURI),
            ext = s".${CompilerAccess.RALPH_FILE_EXTENSION}"
          )
          .map(_.toUri)

      Right(uris)
    } catch TryUtil.catchAllThrows(workspaceURI)

  /** @inheritdoc */
  override def read(fileURI: URI): Either[CompilerMessage.AnyError, String] =
    Using(Source.fromFile(fileURI))(_.mkString) match {
      case Failure(exception) =>
        TryUtil.catchAllThrows(fileURI)(exception)

      case Success(code) =>
        Right(code)
    }

  /** @inheritdoc */
  override def readIfExists(fileURI: URI): Either[CompilerMessage.AnyError, Option[String]] =
    read(fileURI) match {
      case Left(ThrowableError(_, _: FileNotFoundException, _)) =>
        Right(None)

      case other =>
        other.map(Some(_))
    }

  /** @inheritdoc */
  override def write(
      fileURI: URI,
      string: String,
      index: SourceIndex): Either[ThrowableError, Path] =
    try {
      // convert URI to Path
      val filePath = Paths.get(fileURI)
      // ensure directories exists
      Files.createDirectories(filePath.getParent)
      val createdFile = Files.writeString(filePath, string)
      Right(createdFile)
    } catch {
      case throwable: Throwable =>
        val error =
          ThrowableError(
            title = s"Failed to write file '$fileURI'",
            throwable = throwable,
            index = index
          )

        Left(error)
    }

}
