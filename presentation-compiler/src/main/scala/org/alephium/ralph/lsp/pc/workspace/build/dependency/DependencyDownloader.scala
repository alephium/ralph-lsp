package org.alephium.ralph.lsp.pc.workspace.build.dependency

import com.typesafe.scalalogging.LazyLogging
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.pc.sourcecode.imports.StdInterface
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState, RalphcConfig}
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorDownloadingDependency

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

object DependencyDownloader extends LazyLogging {

  /**
   * Download the Std package and return an un-compiled workspace for compilation.
   *
   * @param errorIndex Use this index to report any errors processing the download.
   */
  def downloadStd(errorIndex: SourceIndex): Either[ArraySeq[CompilerMessage.AnyError], WorkspaceState.UnCompiled] =
    downloadStdFromJar(errorIndex) match {
      case Right(source) =>
        // a default build file.
        val build =
          defaultBuildForStd()

        val state =
          WorkspaceState.UnCompiled(
            build = build,
            sourceCode = source.to(ArraySeq)
          )

        Right(state)

      case Left(error) =>
        Left(ArraySeq(error))
    }

  /**
   * Download std code from local jar file.
   *
   * TODO: Downloading source-code should be installable.
   * See issue <a href="https://github.com/alephium/ralph-lsp/issues/44">#44</a>.
   */
  private def downloadStdFromJar(errorIndex: SourceIndex): Either[ErrorDownloadingDependency, Iterable[SourceCodeState.UnCompiled]] =
    try {
      // Errors must be reported to the user. See https://github.com/alephium/ralph-lsp/issues/41.
      val code =
        StdInterface.stdInterfaces map {
          case (path, code) =>
            SourceCodeState.UnCompiled(
              fileURI = path.toUri,
              code = code
            )
        }

      Right(code)
    } catch {
      case throwable: Throwable =>
        val error =
          ErrorDownloadingDependency(
            dependencyID = StdInterface.stdFolder,
            throwable = throwable,
            index = errorIndex
          )

        logger.error(error.title, throwable)

        Left(error)
    }

  /**
   * Currently dependencies do not contain a `ralph.json` file.
   * This function create a default one for the `std` package.
   */
  private def defaultBuildForStd(): BuildState.BuildCompiled = {
    val workspaceDir =
      Paths.get(StdInterface.stdFolder)

    val buildDir =
      workspaceDir.resolve(Build.BUILD_FILE_NAME)

    val compiledConfig =
      org.alephium.ralphc.Config(
        compilerOptions = CompilerOptions.Default,
        contractPath = workspaceDir,
        artifactPath = workspaceDir
      )

    val json =
      RalphcConfig.write(compiledConfig)

    BuildState.BuildCompiled(
      buildURI = buildDir.toUri,
      code = json,
      config = compiledConfig,
      dependency = None
    )
  }
}
