package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.error._
import org.alephium.ralph.lsp.pc.workspace.build.BuildState._
import org.alephium.ralph.lsp.pc.workspace.build.dependency.Dependency

import java.net.URI
import java.nio.file.{Path, Paths}
import scala.collection.immutable.ArraySeq

object Build {

  val BUILD_FILE_EXTENSION = "json"

  /** Build file of a workspace */
  val BUILD_FILE_NAME = s"ralph.$BUILD_FILE_EXTENSION"

  def toBuildPath(workspacePath: Path): Path =
    workspacePath.resolve(BUILD_FILE_NAME)

  def toBuildURI(workspaceURI: URI): URI =
    toBuildPath(Paths.get(workspaceURI)).toUri

  /** Parse a build that is in-memory */
  def parse(buildURI: URI,
            json: String): BuildState.IsParsed =
    RalphcConfig.parse(
      buildURI = buildURI,
      json = json
    ) match {
      case Left(error) =>
        BuildErrored(
          buildURI = buildURI,
          code = Some(json),
          errors = ArraySeq(error),
          dependency = None,
          activateWorkspace = None
        )

      case Right(config) =>
        BuildParsed(
          buildURI = buildURI,
          code = json,
          config = config
        )
    }

  /** Parse a build that is on-disk */
  def parse(buildURI: URI)(implicit file: FileAccess): BuildState.IsParsed =
    file.read(buildURI) match {
      case Left(error) =>
        BuildErrored(
          buildURI = buildURI,
          code = None,
          errors = ArraySeq(error),
          dependency = None,
          activateWorkspace = None
        )

      case Right(json) =>
        parse(
          buildURI = buildURI,
          json = json
        )
    }

  /** Compile a parsed build */
  def compile(parsed: BuildState.IsParsed,
              currentBuild: Option[BuildState.IsCompiled])(implicit file: FileAccess,
                                                           compiler: CompilerAccess,
                                                           logger: ClientLogger): BuildState.IsCompiled =
    parsed match {
      case errored: BuildErrored =>
        // there are parsing errors
        currentBuild match {
          case Some(currentBuild) =>
            // carry the dependency for existing build forward.
            errored.copy(dependency = currentBuild.dependency)

          case None =>
            errored
        }

      case parsed: BuildParsed =>
        def compileDependency() =
          Dependency.compile(
            parsed = parsed,
            currentBuild = currentBuild
          )

        // parse successful. Perform compilation!
        BuildValidator
          .validate(parsed)
          .getOrElse(compileDependency())
    }

  /** Parse and compile from disk */
  def parseAndCompile(buildURI: URI,
                      currentBuild: Option[BuildState.IsCompiled])(implicit file: FileAccess,
                                                                   compiler: CompilerAccess,
                                                                   logger: ClientLogger): BuildState.IsCompiled =
    file.exists(buildURI) match {
      case Left(error) =>
        BuildErrored(
          buildURI = buildURI,
          code = None,
          errors = ArraySeq(error),
          dependency = currentBuild.flatMap(_.dependency),
          activateWorkspace = None
        )

      case Right(exists) =>
        if (exists)
          compile(
            parsed = parse(buildURI),
            currentBuild = currentBuild
          )
        else
          BuildErrored(
            buildURI = buildURI,
            code = None,
            errors = ArraySeq(ErrorBuildFileNotFound),
            dependency = currentBuild.flatMap(_.dependency),
            activateWorkspace = None
          )
    }

  /** Parse and compile from memory */
  def parseAndCompile(buildURI: URI,
                      code: String,
                      currentBuild: Option[BuildState.IsCompiled])(implicit file: FileAccess,
                                                                   compiler: CompilerAccess,
                                                                   logger: ClientLogger): BuildState.IsCompiled = {
    // Code is already read. Parse and validate it.
    val parsed =
      parse(
        buildURI = buildURI,
        json = code
      )

    compile(
      parsed = parsed,
      currentBuild = currentBuild
    )
  }

  def parseAndCompile(buildURI: URI,
                      code: Option[String],
                      currentBuild: Option[BuildState.IsCompiled])(implicit file: FileAccess,
                                                                   compiler: CompilerAccess,
                                                                   logger: ClientLogger): BuildState.IsCompiled =
    code match {
      case Some(code) =>
        parseAndCompile(
          buildURI = buildURI,
          code = code,
          currentBuild = currentBuild
        )

      case None =>
        // Code is not known. Parse and validate it from disk.
        parseAndCompile(
          buildURI = buildURI,
          currentBuild = currentBuild
        )
    }

  /**
   * Parse and re-compile the build file.
   * */
  def parseAndCompile(buildURI: URI,
                      code: Option[String],
                      currentBuild: BuildState.BuildCompiled)(implicit file: FileAccess,
                                                              compiler: CompilerAccess,
                                                              logger: ClientLogger): Option[BuildState.IsCompiled] =
    BuildValidator.validateBuildURI(
      buildURI = buildURI,
      workspaceURI = currentBuild.workspaceURI
    ) match {
      case Left(error) =>
        val buildError =
          BuildState.BuildErrored(
            buildURI = buildURI,
            code = code,
            errors = ArraySeq(error),
            dependency = currentBuild.dependency,
            activateWorkspace = None
          )

        Some(buildError)

      case Right(buildURI) =>
        Build.parseAndCompile(
          buildURI = buildURI,
          code = code,
          currentBuild = Some(currentBuild),
        ) match {
          case newBuild: BuildState.BuildCompiled =>
            // if the new build-file is the same as current build-file, return it as
            // no-state-changed, so that a new build does not unnecessarily gets triggered.
            if (currentBuild == newBuild)
              None
            else // else the build file has changed, return the new build.
              Some(newBuild)

          case errored: BuildState.BuildErrored =>
            Some(errored)
        }
    }

  /**
   * Returns absolute paths of the build/config file.
   *
   * @return A 3-tuple `(workspacePath, absoluteContractPath, absoluteArtifactPath)`
   */
  def getAbsolutePaths(parsed: BuildParsed): (Path, Path, Path, Path) = {
    val workspacePath = Paths.get(parsed.workspaceURI)
    val absoluteContractPath = workspacePath.resolve(Paths.get(parsed.config.contractPath).normalize)
    val absoluteArtifactPath = workspacePath.resolve(Paths.get(parsed.config.artifactPath).normalize)
    val absoluteDependenciesPath = workspacePath.resolve(Paths.get(parsed.config.dependencyPath).normalize)
    (workspacePath, absoluteContractPath, absoluteArtifactPath, absoluteDependenciesPath)
  }

}
