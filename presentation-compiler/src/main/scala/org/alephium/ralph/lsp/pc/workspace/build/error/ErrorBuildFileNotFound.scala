// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}
import org.alephium.ralph.lsp.pc.workspace.build.Build

import java.net.URI

case class ErrorBuildFileNotFound(buildURI: URI) extends CompilerMessage.Error {

  override def message: String =
    s"Build file not found. Create a '${Build.FILE_NAME}' file in the project's root folder."

  override def index: SourceIndex =
    SourceIndexExtra.zero(buildURI)

}
