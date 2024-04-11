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
      BuildState.BuildErrored(
        buildURI = parentBuild.buildURI,
        codeOption = parentBuild.codeOption,
        errors = errors,
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
