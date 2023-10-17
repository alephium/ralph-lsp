package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.workspace.build.RalphcConfig.{RalphcCompiledConfig, RalphcParsedConfig}

import java.net.URI
import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

sealed trait BuildState {
  def buildURI: URI

  def workspaceURI: URI =
    Paths.get(buildURI).getParent.toUri
}

object BuildState {

  /** Parsed states */
  sealed trait Parsed extends BuildState

  /** Compiled states */
  sealed trait Compiled extends BuildState

  /** Build is successfully parsed */
  case class BuildParsed(buildURI: URI,
                         code: String,
                         config: RalphcParsedConfig) extends BuildState.Parsed

  /** Build is successfully compiled */
  case class BuildCompiled(buildURI: URI,
                           code: String,
                           config: RalphcCompiledConfig) extends BuildState.Compiled {
    def contractURI: URI =
      config.contractPath.toUri

    def artifactURI: URI =
      config.artifactPath.toUri
  }

  /** Build errored */
  case class BuildErrored(buildURI: URI,
                          code: Option[String],
                          errors: ArraySeq[CompilerMessage.AnyError]) extends BuildState.Parsed with BuildState.Compiled

}
