package org.alephium.ralph.lsp.access.file

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.error._
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.util.TryUtil
import org.alephium.ralphc.{Compiler => RalphC}

import java.net.URI
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.{Failure, Success, Using}

/**
 * Implements functions accessing on-disk file IO.
 *
 * @note Access to this object is private.
 *       PresentationCompiler does not directly accesses this code.
 */

private object DiskFileAccess extends FileAccess {

  /** @inheritdoc */
  override def exists(fileURI: URI): Either[CompilerMessage.AnyError, Boolean] =
    try
      Right(Files.exists(Paths.get(fileURI)))
    catch {
      case throwable: Throwable =>
        val error =
          ThrowableError(
            title = s"Failed to check if file '$fileURI' exists",
            throwable = throwable
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
          ).map(_.toUri)

      Right(uris)
    } catch TryUtil.catchAllThrows

  /** @inheritdoc */
  override def read(fileURI: URI): Either[CompilerMessage.AnyError, String] =
    Using(Source.fromFile(fileURI))(_.mkString) match {
      case Failure(exception) =>
        TryUtil.catchAllThrows(exception)

      case Success(code) =>
        Right(code)
    }

}
