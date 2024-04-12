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
