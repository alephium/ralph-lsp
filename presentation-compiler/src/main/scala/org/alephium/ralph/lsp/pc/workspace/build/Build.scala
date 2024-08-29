// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, RalphcConfig}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader.DependencyDownloader
import org.alephium.ralph.lsp.pc.workspace.build.dependency.{DependencyDB, Dependency}

import java.net.URI
import java.nio.file.{Path, Paths}
import scala.collection.immutable.ArraySeq

object Build {

  val FILE_EXTENSION = "json"

  /** Build file of a workspace */
  val FILE_NAME = s"ralph.$FILE_EXTENSION"

  /** Directory name where the [[Build.FILE_NAME]] is located */
  private val HOME_DIR_NAME =
    FileAccess.RALPH_LSP_HOME

  /** Constructs the path to the workspace's build directory. */
  def toBuildDir(workspacePath: Path): Path =
    workspacePath.resolve(HOME_DIR_NAME)

  /** Constructs the URI to the workspace's build directory. */
  def toBuildDir(workspaceURI: URI): URI =
    toBuildDir(Paths.get(workspaceURI)).toUri

  /** Constructs the path to the workspace's build file. */
  def toBuildFile(workspacePath: Path): Path =
    toBuildDir(workspacePath).resolve(FILE_NAME)

  /** Constructs the URI to the workspace's build file. */
  def toBuildFile(workspaceURI: URI): URI =
    toBuildFile(Paths.get(workspaceURI)).toUri

  /** Parse a build that is in-memory */
  def parse(
      buildURI: URI,
      json: String): BuildState.IsParsed =
    RalphcConfig.parse(
      buildURI = buildURI,
      json = json
    ) match {
      case Left(error) =>
        BuildState.Errored(
          buildURI = buildURI,
          codeOption = Some(json),
          errors = ArraySeq(error),
          dependencies = ArraySeq.empty,
          activateWorkspace = None
        )

      case Right(config) =>
        BuildState.Parsed(
          buildURI = buildURI,
          code = json,
          config = config
        )
    }

  /** Parse a build that is on-disk */
  def parse(buildURI: URI)(implicit file: FileAccess): BuildState.IsParsed =
    file.read(buildURI) match {
      case Left(error) =>
        BuildState.Errored(
          buildURI = buildURI,
          codeOption = None,
          errors = ArraySeq(error),
          dependencies = ArraySeq.empty,
          activateWorkspace = None
        )

      case Right(json) =>
        parse(
          buildURI = buildURI,
          json = json
        )
    }

  /**
   * Parses the input build JSON if defined, else parses the build from disk
   * using the provided build URI.
   *
   * @param buildURI Location of the build file. Used for reading JSON from disk and reporting errors.
   * @param json     Optional build JSON.
   * @param file     File IO API.
   * @return Parse result.
   */
  def parse(
      buildURI: URI,
      json: Option[String]
    )(implicit file: FileAccess): BuildState.IsParsed =
    json match {
      case Some(json) =>
        parse(
          buildURI = buildURI,
          json = json
        )

      case None =>
        parse(buildURI)
    }

  /** Compile a parsed build */
  def compile(
      parsed: BuildState.IsParsed,
      currentBuild: Option[BuildState.IsCompiled],
      dependencyDownloaders: ArraySeq[DependencyDownloader]
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): BuildState.IsCompiled =
    parsed match {
      case errored: BuildState.Errored =>
        // there are parsing errors
        currentBuild match {
          case Some(currentBuild) =>
            // carry the dependency for existing build forward.
            errored.copy(dependencies = currentBuild.dependencies)

          case None =>
            errored
        }

      case parsed: BuildState.Parsed =>
        def compileDependencies() =
          Dependency.compile(
            parsed = parsed,
            currentBuild = currentBuild,
            downloaders = dependencyDownloaders
          )

        // parse successful. Perform compilation!
        val compilationResult =
          BuildValidator
            .validate(parsed)
            .getOrElse(compileDependencies())

        DependencyDB.persist(
          parentBuild = compilationResult,
          index = getDependantPathIndex(parsed)
        )
    }

  /** Parse and compile from disk */
  def parseAndCompile(
      buildURI: URI,
      currentBuild: Option[BuildState.IsCompiled],
      dependencyDownloaders: ArraySeq[DependencyDownloader]
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): BuildState.IsCompiled =
    file.exists(buildURI, SourceIndexExtra.zero(buildURI)) match {
      case Left(error) =>
        BuildState.Errored(
          buildURI = buildURI,
          codeOption = None,
          errors = ArraySeq(error),
          dependencies = currentBuild.to(ArraySeq).flatMap(_.dependencies),
          activateWorkspace = None
        )

      case Right(exists) =>
        if (exists)
          compile(
            parsed = parse(buildURI),
            currentBuild = currentBuild,
            dependencyDownloaders = dependencyDownloaders
          )
        else
          createDefaultBuildFile(
            buildURI = buildURI,
            currentBuild = currentBuild
          ) match {
            case Some(buildErrored) =>
              buildErrored

            case None =>
              // default build file created! Parse and compile it!
              parseAndCompile(
                buildURI = buildURI,
                currentBuild = currentBuild,
                dependencyDownloaders = dependencyDownloaders
              )
          }
    }

  /** Parse and compile from memory */
  def parseAndCompile(
      buildURI: URI,
      code: String,
      currentBuild: Option[BuildState.IsCompiled],
      dependencyDownloaders: ArraySeq[DependencyDownloader]
    )(implicit file: FileAccess,
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
      currentBuild = currentBuild,
      dependencyDownloaders = dependencyDownloaders
    )
  }

  def parseAndCompile(
      buildURI: URI,
      code: Option[String],
      currentBuild: Option[BuildState.IsCompiled],
      dependencyDownloaders: ArraySeq[DependencyDownloader]
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): BuildState.IsCompiled =
    code match {
      case Some(code) =>
        parseAndCompile(
          buildURI = buildURI,
          code = code,
          currentBuild = currentBuild,
          dependencyDownloaders = dependencyDownloaders
        )

      case None =>
        // Code is not known. Parse and validate it from disk.
        parseAndCompile(
          buildURI = buildURI,
          currentBuild = currentBuild,
          dependencyDownloaders = dependencyDownloaders
        )
    }

  /**
   * Parse and re-compile the build file.
   */
  def parseAndCompile(
      buildURI: URI,
      code: Option[String],
      currentBuild: BuildState.Compiled,
      dependencyDownloaders: ArraySeq[DependencyDownloader]
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Option[BuildState.IsCompiled] =
    BuildValidator.validateBuildURI(
      buildURI = buildURI,
      workspaceURI = currentBuild.workspaceURI
    ) match {
      case Left(error) =>
        val buildError =
          BuildState.Errored(
            buildURI = buildURI,
            codeOption = code,
            errors = ArraySeq(error),
            dependencies = currentBuild.dependencies,
            activateWorkspace = None
          )

        Some(buildError)

      case Right(buildURI) =>
        Build.parseAndCompile(
          buildURI = buildURI,
          code = code,
          currentBuild = Some(currentBuild),
          dependencyDownloaders = dependencyDownloaders
        ) match {
          case newBuild: BuildState.Compiled =>
            // if the new build-file is the same as current build-file, return it as
            // no-state-changed, so that a new build does not unnecessarily gets triggered.
            if (currentBuild == newBuild)
              None
            else // else the build file has changed, return the new build.
              Some(newBuild)

          case errored: BuildState.Errored =>
            Some(errored)
        }
    }

  /**
   * Fetches the parsed build state from the given compiled build state if the compiled build state has no errors.
   *
   * @param build The build to find the parsed state for.
   * @return An [[Option]] containing the parsed build state, or [[None]] if the build contains compilation errors.
   */
  def getParsedOrNone(build: BuildState.IsCompiled)(implicit file: FileAccess): Option[BuildState.Parsed] =
    build match {
      case compiled: BuildState.Compiled =>
        Some(compiled.parsed)

      case errored: BuildState.Errored =>
        // TODO: Currently, a Build's Errored state does not store the `Parsed` state.
        //       There is a need for a `ParsedError` and `CompilationError` split, similar to `SourceCodeState`,
        //       so a `Parsed` state is ALWAYS available for an `IsCompiled` state.
        //       Until then, the following re-parsing of the build JSON is a temporary solution.
        //       Re-parsing is expensive, therefore, implementing the split is necessary.
        parse(
          buildURI = errored.buildURI,
          json = errored.codeOption
        ) match {
          case parsed: BuildState.Parsed =>
            Some(parsed)

          case _: BuildState.Errored =>
            None
        }
    }

  /**
   * Absolute paths of all path settings in the build file.
   *
   * @return A 4-tuple `(workspacePath, absoluteContractPath, (artifactPath, absoluteArtifactPath), dependencyPath)`
   */
  def getAbsolutePaths(parsed: BuildState.Parsed): (Path, Path, Option[(String, Path)], Option[Path]) = {
    val workspacePath            = Paths.get(parsed.workspaceURI)
    val absoluteContractPath     = workspacePath.resolve(Paths.get(parsed.config.contractPath).normalize)
    val absoluteArtifactPath     = getAbsoluteArtifactsPath(parsed)
    val absoluteDependenciesPath = getAbsoluteDependenciesPath(parsed)

    (workspacePath, absoluteContractPath, absoluteArtifactPath, absoluteDependenciesPath)
  }

  /**
   * Absolute paths of contract and artifacts settings in the build file.
   *
   * @return A [[Tuple2]] `(absoluteContractPath, (artifactPath, absoluteArtifactPath))`
   */
  def getAbsoluteContractArtifactPaths(parsed: BuildState.Parsed): (Path, Option[(String, Path)]) = {
    val workspacePath        = Paths.get(parsed.workspaceURI)
    val absoluteContractPath = workspacePath.resolve(Paths.get(parsed.config.contractPath).normalize)
    val absoluteArtifactPath = getAbsoluteArtifactsPath(parsed)

    (absoluteContractPath, absoluteArtifactPath)
  }

  /** Absolute paths of dependencyPath settings in the build file. */
  def getAbsoluteArtifactsPath(parsed: BuildState.Parsed): Option[(String, Path)] =
    parsed.config.artifactPath map {
      artifactPath =>
        val absolute = Paths.get(parsed.workspaceURI).resolve(Paths.get(artifactPath).normalize)
        (artifactPath, absolute)
    }

  /** Absolute paths of dependencyPath settings in the build file. */
  def getAbsoluteDependenciesPath(parsed: BuildState.Parsed): Option[Path] =
    parsed.config.dependencyPath map {
      dependencyPath =>
        Paths.get(parsed.workspaceURI).resolve(Paths.get(dependencyPath).normalize)
    }

  /**
   * Indexes of contractPath, artifactPath and dependencyPathIndex from the ralph.json
   *
   * TODO: This will function will be removed when an AST is available for the JSON.
   */
  def getPathIndexes(parsed: BuildState.Parsed): (SourceIndex, SourceIndex, SourceIndex) = {
    // TODO: lastIndexOf is temporary solution until an AST is available.
    val contractPathIndex =
      SourceIndexExtra.lastIndexOf(
        token = parsed.config.contractPath,
        code = parsed.code,
        fileURI = parsed.buildURI
      )

    val artifactPathIndex =
      getArtifactsPathIndex(parsed)

    val dependencyPathIndex =
      getDependantPathIndex(parsed)

    (contractPathIndex, artifactPathIndex, dependencyPathIndex)
  }

  /** @return Index of the `artifactPath` if configured, or-else the index of the last closing brace. */
  def getArtifactsPathIndex(parsed: BuildState.Parsed): SourceIndex =
    // if artifactPath is None use the index of the last closing brace "}" to report errors
    SourceIndexExtra.lastIndexOf( // TODO: lastIndexOf is temporary solution until an AST is available.
      token = parsed.config.artifactPath getOrElse "}",
      code = parsed.code,
      fileURI = parsed.buildURI
    )

  /** @return Index of the `dependencyPath` if configured, or-else the index of the last closing brace. */
  def getDependantPathIndex(parsed: BuildState.Parsed): SourceIndex =
    // if dependencyPath is None use the index of the last closing brace "}" to report errors
    SourceIndexExtra.lastIndexOf( // TODO: lastIndexOf is temporary solution until an AST is available.
      token = parsed.config.dependencyPath getOrElse "}",
      code = parsed.code,
      fileURI = parsed.buildURI
    )

  /**
   * Generate and persist a default build file.
   *
   * @param buildURI     The location of the build file.
   * @param currentBuild The existing build, used to carry dependencies forward in case of error.
   * @return `Some(error)` if there were IO errors, else `None` for successful creation.
   */
  private def createDefaultBuildFile(
      buildURI: URI,
      currentBuild: Option[BuildState.IsCompiled]
    )(implicit file: FileAccess): Option[BuildState.Errored] =
    file.write(
      fileURI = buildURI,
      string = RalphcConfig.write(RalphcConfigState.Parsed.default, indent = 2),
      index = SourceIndexExtra.zero(buildURI)
    ) match {
      case Left(error) =>
        val buildState =
          BuildState.Errored(
            buildURI = buildURI,
            codeOption = None,
            errors = ArraySeq(error),
            dependencies = currentBuild.to(ArraySeq).flatMap(_.dependencies),
            activateWorkspace = None
          )

        Some(buildState)

      case Right(_) =>
        None
    }

}
