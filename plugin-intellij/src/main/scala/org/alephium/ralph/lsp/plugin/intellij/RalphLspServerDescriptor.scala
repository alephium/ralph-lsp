package org.alephium.ralph.lsp.plugin.intellij

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.platform.lsp.api.ProjectWideLspServerDescriptor

/** A single LSP server that serves the whole project */
class RalphLspServerDescriptor(project: Project) extends ProjectWideLspServerDescriptor(project, "Ralph-LSP") {

  // TODO: Use BuildInfo or an sbt setting to configure this automatically
  private val jar = "/Users/simerplaha/IdeaProjects/ralph-lsp/lsp-server/target/scala-2.13/ralph-lsp.jar"

  def isSupportedFile(file: VirtualFile) = {
    val extension = file.getExtension
    extension == RalphConfig.RALPH_FILE_EXTENSION ||
      extension == RalphConfig.RALPH_BUILD_EXTENSION
  }

  def createCommandLine(): GeneralCommandLine =
    new GeneralCommandLine(System.getProperty("java.home") + "/bin/java", "-jar", jar)

}
