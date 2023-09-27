package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.SourceIndex

import java.net.URI

object ErrorInvalidBuildSyntax {
  /** Build from [[upickle.core.AbortException]] */
  def apply(buildURI: URI,
            error: upickle.core.AbortException): ErrorInvalidBuildSyntax = {
    val errorMessage =
      if (error.clue.isBlank)
        error.getMessage
      else
        error.clue

    ErrorInvalidBuildSyntax(
      fileURI = buildURI,
      index = SourceIndex(error.index, 1),
      message = errorMessage
    )
  }

  /** Build from [[ujson.ParseException]] */
  def apply(buildURI: URI,
            error: ujson.ParseException): ErrorInvalidBuildSyntax = {
    val errorMessage =
      if (error.clue.isBlank)
        error.getMessage
      else
        error.clue

    ErrorInvalidBuildSyntax(
      fileURI = buildURI,
      index = SourceIndex(error.index, 1),
      message = errorMessage
    )
  }

}

case class ErrorInvalidBuildSyntax(fileURI: URI,
                                   index: SourceIndex,
                                   override val message: String) extends FormattableError {
  override def title: String =
    "Syntax"
}
