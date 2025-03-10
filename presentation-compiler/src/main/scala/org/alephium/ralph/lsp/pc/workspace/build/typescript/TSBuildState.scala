// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
