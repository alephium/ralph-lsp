package org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState, RalphcConfig}
import org.alephium.ralph.{CompilerOptions, SourceIndex}

import java.nio.file.Path
import scala.collection.immutable.ArraySeq

object DependencyDownloader extends StrictImplicitLogging {

  /**
   * Download the Std package and return an un-compiled workspace for compilation.
   *
   * @param errorIndex Use this index to report any errors processing the download.
   */
  def downloadStd(dependencyPath: Path,
                  errorIndex: SourceIndex)(implicit logger: ClientLogger): Either[ArraySeq[CompilerMessage.AnyError], WorkspaceState.UnCompiled] =
    StdInterfaceDownloader.stdInterfaces(
      dependencyPath = dependencyPath,
      errorIndex = errorIndex
    ) match {
      case Right(source) =>
        // a default build file.
        val build =
          defaultBuildForStd(dependencyPath)

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
   * Currently dependencies do not contain a `ralph.json` file.
   * This function create a default one for the `std` package.
   */
  private def defaultBuildForStd(dependencyPath: Path): BuildState.BuildCompiled = {
    val workspaceDir =
      dependencyPath resolve StdInterfaceDownloader.stdFolder

    val buildDir =
      workspaceDir resolve Build.BUILD_FILE_NAME

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
      dependency = None,
      dependencyPath = workspaceDir,
      config = compiledConfig
    )
  }
}
