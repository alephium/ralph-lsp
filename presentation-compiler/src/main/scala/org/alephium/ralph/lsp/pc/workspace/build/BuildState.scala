package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.workspace.build.RalphcConfig.{RalphcCompiledConfig, RalphcParsedConfig}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

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
  sealed trait ParseResult extends BuildState

  /** Compiled states */
  sealed trait CompileResult extends BuildState

  /** Build is successfully parsed */
  case class BuildParsed(buildURI: URI,
                         code: String,
                         config: RalphcParsedConfig) extends BuildState.ParseResult

  /** Build is successfully compiled */
  case class BuildCompiled(buildURI: URI,
                           code: String,
                           config: RalphcCompiledConfig) extends BuildState.CompileResult {
    def contractURI: URI =
      config.contractPath.toUri

    def artifactURI: URI =
      config.artifactPath.toUri
  }

  /**
   * Represents: A build error occurred.
   *
   * @param buildURI          Build's location
   * @param code              Build's text content
   * @param errors            Errors to report for the build
   * @param activateWorkspace Workspace to activate as a result of this error.
   *                          This parameter is crucial because even for an invalid build file, the client
   *                          will continue sending source change requests to the server.
   *                          These changes must still be applied to the workspace and carried on to the next compilation.
   *                          Set this to:
   *                          - [[None]] to continue with existing workspace
   *                          - [[WorkspaceState.SourceAware]] to replace existing workspace.
   */
  case class BuildErrored(buildURI: URI,
                          code: Option[String],
                          errors: ArraySeq[CompilerMessage.AnyError],
                          activateWorkspace: Option[WorkspaceState.SourceAware]) extends BuildState.ParseResult with BuildState.CompileResult

}
