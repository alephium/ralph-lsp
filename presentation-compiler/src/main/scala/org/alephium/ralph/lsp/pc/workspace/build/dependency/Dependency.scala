package org.alephium.ralph.lsp.pc.workspace.build.dependency

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader.DependencyDownloader
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorDefaultDependencyDirectoryDoesNotExists
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, Workspace}
import org.alephium.ralphc.Config

import java.nio.file.Path
import scala.collection.immutable.ArraySeq

object Dependency {

  def defaultPath(): Option[Path] =
    FileAccess
      .USER_HOME
      .map(_.resolve(".ralph-lsp").resolve("dependencies"))

  /**
   * Compile this build's dependency.
   *
   * @param parsed       The parsed build of the parent workspace.
   * @param currentBuild Current compiled build.
   * @return
   */
  def compile(
      parsed: BuildState.BuildParsed,
      currentBuild: Option[BuildState.IsCompiled]
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): BuildState.IsCompiled = {
    val absoluteDependenciesPath =
      Build
        .getAbsoluteDependenciesPath(parsed)
        .orElse(defaultPath())

    absoluteDependenciesPath match {
      case Some(absoluteDependenciesPath) =>
        currentBuild match {
          // Check: Does the existing build already has a compiled dependency and is the dependencyPath is unchanged.
          case Some(currentBuild: BuildState.BuildCompiled) if currentBuild.dependencyPath == absoluteDependenciesPath =>
            // Check passed: Re-use the dependency.
            toBuildState(
              parentWorkspaceBuild = parsed,
              dependencyResult = currentBuild.dependencies,
              absoluteDependencyPath = absoluteDependenciesPath
            )

          case _ =>
            // Check failed: Existing code requires a fresh dependency build.
            downloadAndCompileDependencies(
              parsed = parsed,
              absoluteDependencyPath = absoluteDependenciesPath,
              dependencyDownloaders = DependencyDownloader.all()
            )
        }

      case None =>
        val error =
          ErrorDefaultDependencyDirectoryDoesNotExists(
            SourceIndex(
              // since the user did not configure a dependencyPath, report this error at the last closing brace of the build file.
              index = parsed.code.lastIndexOf("}"),
              width = 1,
              Some(parsed.buildURI)
            )
          )

        BuildState.BuildErrored(
          buildURI = parsed.buildURI,
          codeOption = Some(parsed.code),
          errors = ArraySeq(error),
          dependencies = ArraySeq.empty,
          activateWorkspace = None
        )
    }
  }

  /**
   * Download and compile dependencies for a parsed Build [[BuildState.BuildParsed]].
   *
   * @param parsed Build of the parent workspace compiling the dependencies.
   * @return Compilation result contains in the [[BuildState.IsCompiled]] state for
   *         the parent workspace. If there are errors, they will be in
   *         the field [[BuildState.BuildErrored.dependencies]] as a regular workspace errors.
   */
  private def downloadAndCompileDependencies(
      parsed: BuildState.BuildParsed,
      absoluteDependencyPath: Path,
      dependencyDownloaders: ArraySeq[DependencyDownloader]
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): BuildState.IsCompiled = {
    val (errors, downloaded) =
      dependencyDownloaders
        .map {
          downloader =>
            downloader.download(
              dependencyPath = absoluteDependencyPath,
              errorIndex = SourceIndexExtra.zero(parsed.buildURI)
            )
        }
        .partitionMap(identity)

    if (errors.nonEmpty) {
      // report all download errors at build file level.
      BuildState.BuildErrored(
        buildURI = parsed.buildURI,
        codeOption = Some(parsed.code),
        errors = errors.flatten,
        dependencies = ArraySeq.empty,
        activateWorkspace = None
      )
    } else {
      // Compile dependency. A dependency is just a regular workspace with a `ralph.json` file.
      val compiled = downloaded map Workspace.parseAndCompile

      // store it within a compiled build-state
      toBuildState(
        parentWorkspaceBuild = parsed,
        dependencyResult = compiled,
        absoluteDependencyPath = absoluteDependencyPath
      )
    }
  }

  /**
   * Convert the dependency compilation result to  a compiled build-state.
   *
   * @param parentWorkspaceBuild The workspace that is compiling this dependency.
   * @param dependencyResult     Result of dependency compilation
   * @return A compiled result.
   */
  private def toBuildState(
      parentWorkspaceBuild: BuildState.BuildParsed,
      dependencyResult: ArraySeq[WorkspaceState.IsParsedAndCompiled],
      absoluteDependencyPath: Path): BuildState.IsCompiled = {
    val compiledResults =
      dependencyResult collect {
        case compiledDependency: WorkspaceState.Compiled =>
          compiledDependency
      }

    if (compiledResults.length == dependencyResult.length) {
      val (absoluteContractPath, absoluteArtifactPath) =
        Build.getAbsoluteContractArtifactPaths(parentWorkspaceBuild)

      val config =
        Config(
          compilerOptions = parentWorkspaceBuild.config.compilerOptions,
          contractPath = absoluteContractPath,
          artifactPath = absoluteArtifactPath
        )

      // Build OK. Promote build to compiled state.
      BuildState.BuildCompiled(
        buildURI = parentWorkspaceBuild.buildURI,
        code = parentWorkspaceBuild.code,
        dependencies = compiledResults, // store the compiled dependency in the build.
        dependencyPath = absoluteDependencyPath,
        config = config
      )
    } else {
      BuildState.BuildErrored(
        buildURI = parentWorkspaceBuild.buildURI,
        codeOption = Some(parentWorkspaceBuild.code),
        errors = ArraySeq.empty,
        dependencies = dependencyResult, // dependency workspace with error.
        activateWorkspace = None
      )
    }
  }

}
