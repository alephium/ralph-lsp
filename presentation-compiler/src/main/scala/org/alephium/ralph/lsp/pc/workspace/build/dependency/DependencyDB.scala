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

package org.alephium.ralph.lsp.pc.workspace.build.dependency

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.build.BuildState

import java.nio.file.{Path, Paths}
import scala.collection.immutable.ArraySeq

object DependencyDB extends StrictImplicitLogging {

  /**
   * Persist dependencies of a compiled build.
   *
   * @param parentBuild Build of the parent workspace.
   * @return Compiled compiled or build errors.
   */

  def persist(
      parentBuild: BuildState.IsCompiled,
      index: SourceIndex
    )(implicit file: FileAccess,
      logger: ClientLogger): BuildState.IsCompiled = {
    val (errors, _) =
      parentBuild
        .dependencies
        .flatMap(_.sourceCode)
        .collect {
          case source: SourceCodeState.IsCodeAware =>
            persistSource(source, index)
        }
        .partitionMap(identity)

    if (errors.nonEmpty)
      BuildState.Errored(
        buildURI = parentBuild.buildURI,
        codeOption = parentBuild.codeOption,
        errors = errors,
        tsState = None,
        dependencies = ArraySeq.empty,
        activateWorkspace = None
      )
    else
      parentBuild
  }

  private def persistSource(
      source: SourceCodeState.IsCodeAware,
      index: SourceIndex
    )(implicit file: FileAccess,
      logger: ClientLogger): Either[CompilerMessage.AnyError, Path] =
    file.exists(
      fileURI = source.fileURI,
      index = index
    ) flatMap {
      exists =>
        if (!exists) {
          logger.trace(s"Writing dependency code. URI: ${source.fileURI}")
          file.write(
            fileURI = source.fileURI,
            string = source.code,
            index = index
          )
        } else {
          logger.trace(s"Dependency code already exists. URI: ${source.fileURI}")
          Right(Paths.get(source.fileURI))
        }
    }

}
