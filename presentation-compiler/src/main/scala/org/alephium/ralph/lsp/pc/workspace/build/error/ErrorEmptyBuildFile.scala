// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}
import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, RalphcConfig}

import java.net.URI

case class ErrorEmptyBuildFile(fileURI: URI) extends CompilerMessage.Error {

  override def message: String = {
    val defaultBuild = RalphcConfig.write(RalphcConfigState.Parsed.default)

    s"""Empty build file detected. Consider copying the following default build JSON or refer to the documentation for guidance.
         |$defaultBuild""".stripMargin
  }

  override def index: SourceIndex =
    SourceIndexExtra.zero(fileURI)

}
