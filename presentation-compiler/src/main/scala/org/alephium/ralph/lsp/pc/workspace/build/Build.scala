package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.SourceIndex
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.BuildState._
import org.alephium.ralph.lsp.pc.workspace.build.dependency.{Dependency, DependencyDB}
import org.alephium.ralph.lsp.pc.workspace.build.error._

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
        val compilationResult =
          BuildValidator
            .validate(parsed)
            .getOrElse(compileDependency())

        DependencyDB.persist(
          parentBuild = compilationResult,
          index = getDependantPathIndex(parsed)
        )
    }

  /** Parse and compile from disk */
  def parseAndCompile(buildURI: URI,
                      currentBuild: Option[BuildState.IsCompiled])(implicit file: FileAccess,
                                                                   compiler: CompilerAccess,
                                                                   logger: ClientLogger): BuildState.IsCompiled =
    file.exists(buildURI, SourceIndex.empty) match {
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
   * Absolute paths of all path settings in the build file.
   *
   * @return A 4-tuple `(workspacePath, absoluteContractPath, absoluteArtifactPath, dependencyPath)`
   */
  def getAbsolutePaths(parsed: BuildParsed): (Path, Path, Path, Option[Path]) = {
    val workspacePath = Paths.get(parsed.workspaceURI)
    val absoluteContractPath = workspacePath.resolve(Paths.get(parsed.config.contractPath).normalize)
    val absoluteArtifactPath = workspacePath.resolve(Paths.get(parsed.config.artifactPath).normalize)
    val absoluteDependenciesPath = getAbsoluteDependenciesPath(parsed)

    (workspacePath, absoluteContractPath, absoluteArtifactPath, absoluteDependenciesPath)
  }

  /** Absolute paths of contract and artifacts settings in the build file. */
  def getAbsoluteContractArtifactPaths(parsed: BuildParsed): (Path, Path) = {
    val workspacePath = Paths.get(parsed.workspaceURI)
    val absoluteContractPath = workspacePath.resolve(Paths.get(parsed.config.contractPath).normalize)
    val absoluteArtifactPath = workspacePath.resolve(Paths.get(parsed.config.artifactPath).normalize)

    (absoluteContractPath, absoluteArtifactPath)
  }

  /** Absolute paths of dependencyPath settings in the build file. */
  def getAbsoluteDependenciesPath(parsed: BuildParsed): Option[Path] =
    parsed.config.dependencyPath map {
      dependencyPath =>
        Paths.get(parsed.workspaceURI).resolve(Paths.get(dependencyPath).normalize)
    }

  /**
   * Indexes of contractPath, artifactPath and dependencyPathIndex from the ralph.json
   *
   * TODO: This will function will be removed when an AST is available for the JSON.
   * */
  def getPathIndexes(parsed: BuildParsed): (SourceIndex, SourceIndex, SourceIndex) = {
    val contractPath = parsed.config.contractPath
    val artifactPath = parsed.config.artifactPath

    val contractPathIndex =
      SourceIndex.ensurePositive(
        index = parsed.code.lastIndexOf(contractPath), // TODO: lastIndexOf is temporary solution until an AST is available.
        width = contractPath.length
      )

    val artifactPathIndex =
      SourceIndex.ensurePositive(
        index = parsed.code.lastIndexOf(artifactPath), // TODO: lastIndexOf is temporary solution until an AST is available.
        width = artifactPath.length
      )

    val dependencyPathIndex =
      getDependantPathIndex(parsed)

    (contractPathIndex, artifactPathIndex, dependencyPathIndex)
  }

  /** @return Index of the `dependencyPath` if configured, or-else the index of the last closing brace. */
  def getDependantPathIndex(parsed: BuildParsed): SourceIndex = {
    // if dependencyPath is None use the index of the last closing brace "}" to report errors
    val errorIndexToken =
      parsed.config.dependencyPath getOrElse "}"

    SourceIndex.ensurePositive(
      index = parsed.code.lastIndexOf(errorIndexToken), // TODO: lastIndexOf is temporary solution until an AST is available.
      width = errorIndexToken.length
    )
  }

}
