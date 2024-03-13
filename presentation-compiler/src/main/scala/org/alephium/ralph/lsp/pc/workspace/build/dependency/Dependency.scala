package org.alephium.ralph.lsp.pc.workspace.build.dependency

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorDefaultDependencyDirectoryDoesNotExists
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState}
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceState}
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
  def compile(parsed: BuildState.BuildParsed,
              currentBuild: Option[BuildState.IsCompiled])(implicit file: FileAccess,
                                                           compiler: CompilerAccess,
                                                           logger: ClientLogger): BuildState.IsCompiled = {
    val absoluteDependenciesPath =
      Build
        .getAbsoluteDependenciesPath(parsed)
        .orElse(defaultPath())

    absoluteDependenciesPath match {
      case Some(absoluteDependenciesPath) =>
        currentBuild.flatMap(_.dependency) match {
          case Some(dependency) if absoluteDependenciesPath == dependency.build.dependencyPath =>
            // Existing build already has compiled dependency and dependencyPath is unchanged. Re-use the dependency.
            toBuildState(
              parentWorkspaceBuild = parsed,
              dependencyResult = dependency,
              absoluteDependenciesPath = absoluteDependenciesPath
            )

          case _ =>
            // Existing code requires a dependency build.
            downloadAndCompileStd(
              parsed = parsed,
              absoluteDependenciesPath = absoluteDependenciesPath
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
          code = Some(parsed.code),
          errors = ArraySeq(error),
          dependency = None,
          activateWorkspace = None
        )
    }
  }

  /**
   * Download and compile standard/std dependency for a parsed Build [[BuildState.BuildParsed]].
   *
   * @param parsed Build of the parent workspace compiling this standard/std dependency.
   * @return Compilation result contains in the [[BuildState.IsCompiled]] state for
   *         the parent workspace. If there are errors, they will be in
   *         the field [[BuildState.BuildErrored.dependency]] as a regular workspace errors.
   */
  private def downloadAndCompileStd(parsed: BuildState.BuildParsed,
                                    absoluteDependenciesPath: Path)(implicit file: FileAccess,
                                                                    compiler: CompilerAccess,
                                                                    logger: ClientLogger): BuildState.IsCompiled =
    DependencyDownloader.downloadStd(
      dependencyPath = absoluteDependenciesPath,
      errorIndex = SourceIndexExtra.zero(parsed.buildURI)
    ) match { // download std
      case Left(errors) =>
        // report all download errors at build file level.
        BuildState.BuildErrored(
          buildURI = parsed.buildURI,
          code = Some(parsed.code),
          errors = errors,
          dependency = None,
          activateWorkspace = None
        )

      case Right(dependencyStd) =>
        // Compile std. A dependency is just a regular workspace with a `ralph.json` file.
        val dependencyStdCompiled =
          Workspace.parseAndCompile(dependencyStd)

        // store it within a compiled build-state
        toBuildState(
          parentWorkspaceBuild = parsed,
          dependencyResult = dependencyStdCompiled,
          absoluteDependenciesPath = absoluteDependenciesPath
        )
    }

  /**
   * Convert the dependency compilation result to  a compiled build-state.
   *
   * @param parentWorkspaceBuild The workspace that is compiling this dependency.
   * @param dependencyResult     Result of dependency compilation
   * @return A compiled result.
   */
  private def toBuildState(parentWorkspaceBuild: BuildState.BuildParsed,
                           dependencyResult: WorkspaceState.IsParsedAndCompiled,
                           absoluteDependenciesPath: Path): BuildState.IsCompiled =
    dependencyResult match {
      case compiledStd: WorkspaceState.Compiled => // Dependency compiled OK. Convert the build state to compiled.
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
          dependency = Some(compiledStd), // store the compiled dependency in the build.
          dependencyPath = absoluteDependenciesPath,
          config = config
        )

      case state @ (_: WorkspaceState.Errored | _: WorkspaceState.UnCompiled) =>
        // Dependency code has errors. Return error build state and store the errored workspace.
        BuildState.BuildErrored(
          buildURI = parentWorkspaceBuild.buildURI,
          code = Some(parentWorkspaceBuild.code),
          errors = ArraySeq.empty,
          dependency = Some(state), // dependency workspace with error.
          activateWorkspace = None
        )
    }
}
