package org.alephium.ralph.lsp.plugin.intellij

import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.platform.lsp.api.LspServerSupportProvider

/** Add LSP support to IntelliJ */
class RalphLspServerSupportProvider extends LspServerSupportProvider {

  /** Start an LSP server lazily */
  override def fileOpened(project: Project, file: VirtualFile, serverStarter: LspServerSupportProvider.LspServerStarter): Unit =
    if (file.getExtension == "ral")
      serverStarter.ensureServerStarted(new RalphLspServerDescriptor(project))
}
