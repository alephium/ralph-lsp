package org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorEmptyErrorsOnDownload
import org.alephium.ralph.lsp.pc.workspace.build.{RalphcConfig, Build, BuildState}
import org.alephium.ralph.{SourceIndex, CompilerOptions}

import java.nio.file.Path
import scala.collection.immutable.ArraySeq

/**
 * A dependency downloader, responsible for downloading code dependencies into an un-compiled workspace.
 */
trait DependencyDownloader extends StrictImplicitLogging { self =>

  /**
   * Downloads dependency code to an un-compiled workspace.
   *
   * @param dependencyPath The configured dependency directory.
   * @param errorIndex     Index to report downloading errors.
   * @return Either an array of downloading errors or an un-compiled workspace on successful download.
   */
  protected def _download(
      dependencyPath: Path,
      errorIndex: SourceIndex
    )(implicit logger: ClientLogger): Either[ArraySeq[CompilerMessage.AnyError], WorkspaceState.UnCompiled]

  /**
   * Invokes the function [[_download]], checking that errors are non-empty.
   *
   * @param dependencyPath The configured dependency directory.
   * @param errorIndex     Index to report downloading errors.
   * @return Either an array of downloading errors or an un-compiled workspace on successful download.
   */
  final def download(
      dependencyPath: Path,
      errorIndex: SourceIndex
    )(implicit logger: ClientLogger): Either[ArraySeq[CompilerMessage.AnyError], WorkspaceState.UnCompiled] =
    _download(
      dependencyPath = dependencyPath,
      errorIndex = errorIndex
    ) match {
      case left @ Left(errors) =>
        // Log when the downloader results in empty errors.
        // This is to detect scenarios where compilation stalls due to a bug, leading to no error reported to the editor.
        // This log should help in tracing that bug.
        if (errors.isEmpty) {
          val error = ErrorEmptyErrorsOnDownload(self, errorIndex)
          logger.error(error.message)
          Left(ArraySeq(error))
        } else {
          left
        }

      case right @ Right(_) =>
        right
    }

}

object DependencyDownloader {

  /** All dependency downloaders */
  def all(): ArraySeq[DependencyDownloader] =
    ArraySeq(
      StdInterfaceDownloader,
      BuiltInFunctionDownloader
    )

  /**
   * Currently dependencies do not contain a `ralph.json` file.
   * This function create a default file.
   *
   * @param workspaceDir The directory of the workspace for which this build file is being created.
   * @return Compiled build state representing the default `ralph.json`.
   */
  def defaultBuild(workspaceDir: Path): BuildState.Compiled = {
    val buildDir =
      workspaceDir resolve Build.BUILD_FILE_NAME

    val compiledConfig =
      org
        .alephium
        .ralphc
        .Config(
          compilerOptions = CompilerOptions.Default,
          contractPath = workspaceDir,
          artifactPath = workspaceDir
        )

    val json =
      RalphcConfig.write(compiledConfig)

    BuildState.Compiled(
      buildURI = buildDir.toUri,
      code = json,
      dependencies = ArraySeq.empty,
      dependencyPath = workspaceDir,
      config = compiledConfig
    )
  }

}
