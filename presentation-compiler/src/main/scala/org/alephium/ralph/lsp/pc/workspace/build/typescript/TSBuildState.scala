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

package org.alephium.ralph.lsp.pc.workspace.build.typescript

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage

import java.net.URI
import scala.collection.immutable.ArraySeq

sealed trait TSBuildState {

  def buildURI: URI

  def code: Option[String]

}

object TSBuildState {

  /**
   * Represents the errored state of the TypeScript build.
   *
   * @param buildURI The URI of the build.
   * @param code     Optional code content.
   * @param errors   Array of compiler errors.
   */
  case class Errored(
      buildURI: URI,
      code: Option[String],
      errors: ArraySeq[CompilerMessage.AnyError])
    extends TSBuildState

}
