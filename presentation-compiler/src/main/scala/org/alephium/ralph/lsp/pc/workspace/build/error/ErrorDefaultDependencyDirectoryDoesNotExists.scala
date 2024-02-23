package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.pc.workspace.build.Build

case class ErrorDefaultDependencyDirectoryDoesNotExists(index: SourceIndex) extends CompilerMessage.Error {

  override def message: String =
    s"""Unable to write dependencies because `dependencyPath` or the system property `user.home` is not configured.
       |To fix this, either add the setting "dependencyPath": "dependencies" in ${Build.BUILD_FILE_NAME} and create a directory named "dependencies" in your project, or set the system property `user.home`.
       |""".stripMargin

}
