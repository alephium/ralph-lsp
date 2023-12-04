package org.alephium.ralph.lsp.pc.workspace.build.dependency

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.error.StringError
import org.alephium.ralph.lsp.access.compiler.message.SourceIndex
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.{Workspace, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState._
import org.alephium.ralphc.Config

import scala.collection.immutable.ArraySeq

object Dependency {

  /**
   * Compile this build's dependency.
   *
   * @param parsed       The parsed build of the parent workspace.
   * @param currentBuild Current compiled build.
   * @return
   */
  def compile(parsed: BuildParsed,
              currentBuild: Option[BuildState.IsCompiled])(implicit file: FileAccess,
                                                           compiler: CompilerAccess): BuildState.IsCompiled =
    currentBuild.flatMap(_.dependency) match {
      case Some(dependency) =>
        // Existing build already has compiled dependency. Re-use it.
        toBuildState(
          parentWorkspaceBuild = parsed,
          dependencyResult = dependency
        )

      case None =>
        // Existing code requires a dependency build.
        downloadAndCompileStd(parsed)
    }

  /**
   * Download and compile standard/std dependency for a parsed Build [[BuildState.BuildParsed]].
   *
   * @param parsed Build of the parent workspace compiling this standard/std dependency.
   * @return Compilation result contains in the [[BuildState.IsCompiled]] state for
   *         the parent workspace. If there are errors, they will be in
   *         the field [[BuildState.BuildErrored.dependency]] as a regular workspace errors.
   */
  def downloadAndCompileStd(parsed: BuildParsed)(implicit file: FileAccess,
                                                 compiler: CompilerAccess): BuildState.IsCompiled =
    DependencyDownloader.downloadStd(errorIndex = SourceIndex.empty) match { // download std
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
          dependencyResult = dependencyStdCompiled
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
                           dependencyResult: WorkspaceState.IsSourceAware): BuildState.IsCompiled =
    dependencyResult match {
      case compiledStd: WorkspaceState.Compiled => // Dependency compiled OK. Convert the build state to compiled.
        val (_, absoluteContractPath, absoluteArtifactPath) =
          Build.getAbsolutePaths(parentWorkspaceBuild)

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

      case parsed: WorkspaceState.Parsed =>
        // FIXME: This state should NEVER be the case in reality. A parsed state is ALWAYS compilable.
        //        Stronger types are needed for [[Workspace.parseAndCompile]] that disallow it from returning
        //        a parsed state as its result.
        val error =
          StringError("Failed to compile dependency. Compilation resulted in a parsed state.")

        BuildState.BuildErrored(
          buildURI = parsed.buildURI,
          code = Some(parsed.build.code),
          errors = ArraySeq(error),
          dependency = None,
          activateWorkspace = None
        )
    }
}
