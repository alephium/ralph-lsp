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

package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.RalphcConfig.{RalphcCompiledConfig, RalphcParsedConfig}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID

import java.net.URI
import java.nio.file.{Path, Paths}
import scala.collection.immutable.ArraySeq

sealed trait BuildState {

  def codeOption: Option[String]

  def buildURI: URI

  def workspaceURI: URI =
    Paths.get(buildURI).getParent.toUri

}

object BuildState {

  /** Parsed states */
  sealed trait IsParsed extends BuildState

  /** Compiled states */
  sealed trait IsCompiled extends BuildState {

    def dependencies: ArraySeq[WorkspaceState.IsParsedAndCompiled]

    def findDependency(id: DependencyID): Option[WorkspaceState.IsParsedAndCompiled] =
      BuildState.findDependency(dependencies, id)

  }

  /** Build is successfully parsed */
  case class Parsed(
      buildURI: URI,
      code: String,
      config: RalphcParsedConfig)
    extends BuildState.IsParsed {

    override def codeOption: Option[String] =
      Some(code)

  }

  /** Build is successfully compiled */
  case class Compiled(
      buildURI: URI,
      code: String,
      dependencies: ArraySeq[WorkspaceState.Compiled],
      dependencyPath: Path,
      config: RalphcCompiledConfig)
    extends BuildState.IsCompiled {

    def contractURI: URI =
      config.contractPath.toUri

    def artifactURI: URI =
      config.artifactPath.toUri

    override def findDependency(id: DependencyID): Option[WorkspaceState.Compiled] =
      BuildState.findDependency(dependencies, id)

    override def codeOption: Option[String] =
      Some(code)

  }

  /**
   * Represents: A build error occurred.
   *
   * @param buildURI          Build's location
   * @param codeOption        Build's text content
   * @param errors            Errors to report for the build
   * @param activateWorkspace Workspace to activate as a result of this error.
   *                          This parameter is crucial because even for an invalid build file, the client
   *                          will continue sending source change requests to the server.
   *                          These changes must still be applied to the workspace and carried on to the next compilation.
   *                          Set this to:
   *                          - [[None]] to continue with existing workspace
   *                          - Some [[WorkspaceState.IsSourceAware]] to replace existing workspace.
   */
  case class Errored(
      buildURI: URI,
      codeOption: Option[String],
      errors: ArraySeq[CompilerMessage.AnyError],
      dependencies: ArraySeq[WorkspaceState.IsParsedAndCompiled],
      activateWorkspace: Option[WorkspaceState.IsSourceAware])
    extends BuildState.IsParsed
       with BuildState.IsCompiled

  /**
   * Finds a dependency with the specified ID in the given array of dependencies.
   *
   * @param dependencies The array of dependencies to search within.
   * @param id           The ID of the dependency to find.
   * @return An option containing the found dependency, or None if not found.
   */
  @inline private def findDependency[T <: WorkspaceState.IsParsedAndCompiled](
      dependencies: ArraySeq[T],
      id: DependencyID): Option[T] =
    dependencies find {
      dependency =>
        URIUtil.getFileName(dependency.workspaceURI) == id.dirName
    }

}
