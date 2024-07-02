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
import org.alephium.ralph.lsp.pc.workspace.build.dependency.{DependencyDB, Dependency}

import java.net.URI
import java.nio.file.{Path, Paths}
import scala.collection.immutable.ArraySeq

object Build {

  val BUILD_FILE_EXTENSION = "json"

  /** Build file of a workspace */
  val FILE_NAME = s"ralph.$BUILD_FILE_EXTENSION"

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

  /** Compile a parsed build */
  def compile(
      parsed: BuildState.IsParsed,
      currentBuild: Option[BuildState.IsCompiled]
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
            currentBuild = currentBuild
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
      currentBuild: Option[BuildState.IsCompiled]
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
            currentBuild = currentBuild
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
                currentBuild = currentBuild
              )
          }
    }

  /** Parse and compile from memory */
  def parseAndCompile(
      buildURI: URI,
      code: String,
      currentBuild: Option[BuildState.IsCompiled]
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
      currentBuild = currentBuild
    )
  }

  def parseAndCompile(
      buildURI: URI,
      code: Option[String],
      currentBuild: Option[BuildState.IsCompiled]
    )(implicit file: FileAccess,
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
   */
  def parseAndCompile(
      buildURI: URI,
      code: Option[String],
      currentBuild: BuildState.Compiled
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
          currentBuild = Some(currentBuild)
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
   * Absolute paths of all path settings in the build file.
   *
   * @return A 4-tuple `(workspacePath, absoluteContractPath, absoluteArtifactPath, dependencyPath)`
   */
  def getAbsolutePaths(parsed: BuildState.Parsed): (Path, Path, Path, Option[Path]) = {
    val workspacePath            = Paths.get(parsed.workspaceURI)
    val absoluteContractPath     = workspacePath.resolve(Paths.get(parsed.config.contractPath).normalize)
    val absoluteArtifactPath     = workspacePath.resolve(Paths.get(parsed.config.artifactPath).normalize)
    val absoluteDependenciesPath = getAbsoluteDependenciesPath(parsed)

    (workspacePath, absoluteContractPath, absoluteArtifactPath, absoluteDependenciesPath)
  }

  /** Absolute paths of contract and artifacts settings in the build file. */
  def getAbsoluteContractArtifactPaths(parsed: BuildState.Parsed): (Path, Path) = {
    val workspacePath        = Paths.get(parsed.workspaceURI)
    val absoluteContractPath = workspacePath.resolve(Paths.get(parsed.config.contractPath).normalize)
    val absoluteArtifactPath = workspacePath.resolve(Paths.get(parsed.config.artifactPath).normalize)

    (absoluteContractPath, absoluteArtifactPath)
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
    val contractPath = parsed.config.contractPath
    val artifactPath = parsed.config.artifactPath

    val contractPathIndex =
      SourceIndexExtra.ensurePositive(
        index = parsed.code.lastIndexOf(contractPath), // TODO: lastIndexOf is temporary solution until an AST is available.
        width = contractPath.length,
        fileURI = parsed.buildURI
      )

    val artifactPathIndex =
      SourceIndexExtra.ensurePositive(
        index = parsed.code.lastIndexOf(artifactPath), // TODO: lastIndexOf is temporary solution until an AST is available.
        width = artifactPath.length,
        fileURI = parsed.buildURI
      )

    val dependencyPathIndex =
      getDependantPathIndex(parsed)

    (contractPathIndex, artifactPathIndex, dependencyPathIndex)
  }

  /** @return Index of the `dependencyPath` if configured, or-else the index of the last closing brace. */
  def getDependantPathIndex(parsed: BuildState.Parsed): SourceIndex = {
    // if dependencyPath is None use the index of the last closing brace "}" to report errors
    val errorIndexToken =
      parsed.config.dependencyPath getOrElse "}"

    SourceIndexExtra.ensurePositive(
      index = parsed.code.lastIndexOf(errorIndexToken), // TODO: lastIndexOf is temporary solution until an AST is available.
      width = errorIndexToken.length,
      fileURI = parsed.buildURI
    )
  }

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
      string = RalphcConfig.write(RalphcConfig.defaultParsedConfig, indent = 2),
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
