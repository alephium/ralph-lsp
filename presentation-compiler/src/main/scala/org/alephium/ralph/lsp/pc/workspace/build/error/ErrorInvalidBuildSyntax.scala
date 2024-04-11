package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage

import java.net.URI

object ErrorInvalidBuildSyntax {

  /** Build from [[upickle.core.AbortException]] */
  def apply(
      buildURI: URI,
      error: upickle.core.AbortException): ErrorInvalidBuildSyntax = {
    val errorMessage =
      if (error.clue.isBlank)
        error.getMessage
      else
        error.clue

    ErrorInvalidBuildSyntax(
      fileURI = buildURI,
      index = SourceIndex(error.index, 1, Some(buildURI)),
      message = errorMessage
    )
  }

  /** Build from [[ujson.ParseException]] */
  def apply(
      buildURI: URI,
      error: ujson.ParseException): ErrorInvalidBuildSyntax = {
    val errorMessage =
      if (error.clue.isBlank)
        error.getMessage
      else
        error.clue

    ErrorInvalidBuildSyntax(
      fileURI = buildURI,
      index = SourceIndex(error.index, 1, Some(buildURI)),
      message = errorMessage
    )
  }

}

/*
 * fileURI is redundant with the one in `index`, but as long as `SourceIndex.fileURI` is optional
 * it's better to keep it.
 */
case class ErrorInvalidBuildSyntax(
    fileURI: URI,
    index: SourceIndex,
    override val message: String)
  extends CompilerMessage.Error
