package org.alephium.ralph.lsp.pc.workspace.build.dependency

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.build.BuildState

import java.nio.file.{Path, Paths}

object DependencyDB extends StrictImplicitLogging {

  /**
   * Persist dependencies of a compiled build.
   *
   * @param parentBuild Build of the parent workspace.
   * @return Compiled compiled or build errors.
   */
  def persist(parentBuild: BuildState.IsCompiled,
              index: SourceIndex)(implicit file: FileAccess,
                                  logger: ClientLogger): BuildState.IsCompiled =
    parentBuild match {
      case build: BuildState.BuildCompiled =>
        persistCompiled(
          build = build,
          index = index
        )

      case errored: BuildState.BuildErrored =>
        errored
    }

  private def persistCompiled(build: BuildState.BuildCompiled,
                              index: SourceIndex)(implicit file: FileAccess,
                                                  logger: ClientLogger): BuildState.IsCompiled = {
    val result =
      build
        .dependency
        .map(_.sourceCode.map(persistSource(_, index)))

    result match {
      case Some(result) =>
        val (errors, _) =
          result partitionMap identity

        if (errors.nonEmpty)
          BuildState.BuildErrored(
            buildURI = build.buildURI,
            code = Some(build.code),
            errors = errors,
            dependency = None,
            activateWorkspace = None
          )
        else
          build

      case None =>
        build
    }
  }

  private def persistSource(source: SourceCodeState.Compiled,
                            index: SourceIndex)(implicit file: FileAccess,
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
