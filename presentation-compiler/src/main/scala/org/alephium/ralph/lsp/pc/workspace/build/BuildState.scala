package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralphc.Config

import java.net.URI
import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

sealed trait BuildState

object BuildState {

  /** State: Build file is compiled. This state can upgraded to [[UnCompiled]]. */
  case class BuildCompiled(buildURI: URI,
                           code: String,
                           config: Config) extends BuildState {
    val workspaceURI: URI =
      Paths.get(buildURI).getParent.toUri


    def contractURI: URI =
      workspaceURI.resolve(s"${config.contractPath}")

    def artifactURI: URI =
      workspaceURI.resolve(s"${config.artifactPath}")
  }

  case class BuildErrored(buildURI: URI,
                          code: Option[String],
                          errors: ArraySeq[FormattableError]) extends BuildState

}
