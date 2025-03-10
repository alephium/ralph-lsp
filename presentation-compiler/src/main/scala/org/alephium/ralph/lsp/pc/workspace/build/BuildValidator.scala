// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.build.dependency.Dependency
import org.alephium.ralph.lsp.pc.workspace.build.error._
import org.alephium.ralph.lsp.utils.URIUtil

import java.net.URI
import java.nio.file.Path
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer

/** Implements functions for validating `ralph.json` */
object BuildValidator {

  /** Validate and promotes a parsed build-file to compiled */
  def validate(parsed: BuildState.Parsed)(implicit file: FileAccess): Option[BuildState.Errored] =
    validatePathsInWorkspace(parsed) // Run validation checks
      .orElse(validatePathsExists(parsed))

  /** Checks that buildURI is in the project's root directory */
  def validateBuildURI(
      buildURI: URI,
      workspaceURI: URI): Either[CompilerMessage.Error, URI] =
    if (!URIUtil.isFileName(buildURI, Build.FILE_NAME))
      Left(ErrorBuildFileNotFound(buildURI))
    else if (!URIUtil.isFirstChild(Build.toBuildDir(workspaceURI), buildURI)) // Build file must be in the root workspace directory.
      Left(
        ErrorInvalidBuildFileLocation(
          buildURI = buildURI,
          workspaceURI = workspaceURI
        )
      )
    else
      Right(buildURI)

  /** Validate that the configured paths are within the workspace directory */
  private def validatePathsInWorkspace(parsed: BuildState.Parsed): Option[BuildState.Errored] = {
    // absolute source paths
    val (workspacePath, absoluteContractPath, absoluteArtifactPath, absoluteDependencyPath) =
      Build.getAbsolutePaths(parsed)

    val (contractPathIndex, artifactPathIndex, dependencyPathIndex) =
      Build.getPathIndexes(parsed)

    val errors =
      ListBuffer.empty[CompilerMessage.AnyError]

    // Validate: is the contract path within the workspace
    if (!URIUtil.contains(workspacePath, absoluteContractPath))
      errors addOne
        ErrorDirectoryOutsideWorkspace(
          dirPath = parsed.config.contractPath,
          index = contractPathIndex
        )

    // Validate: is the artifact path within the workspace
    absoluteArtifactPath foreach {
      case (artifactPath, absoluteArtifactPath) =>
        if (!URIUtil.contains(workspacePath, absoluteArtifactPath))
          errors addOne
            ErrorDirectoryOutsideWorkspace(
              dirPath = artifactPath,
              index = artifactPathIndex
            )
    }

    // Validate: DependencyPath and ContractPath should not overlap
    absoluteDependencyPath foreach {
      absoluteDependencyPath =>
        if (URIUtil.contains(absoluteContractPath, absoluteDependencyPath) || URIUtil.contains(absoluteDependencyPath, absoluteContractPath)) {
          // TODO: When `contractPath` and `dependencyPath` are identical then both errors will have the same index, which means
          //       both errors get reported at the same field. This will be fixed when an AST is available in issue #17.
          errors addOne ErrorDependencyPathIsWithinContractPath(index = contractPathIndex)
          errors addOne ErrorDependencyPathIsWithinContractPath(index = dependencyPathIndex)
        }
    }

    // Check if errors exists
    if (errors.isEmpty)
      None
    else
      Some(
        BuildState.Errored( // report errors
          buildURI = parsed.buildURI,
          codeOption = Some(parsed.code),
          errors = ArraySeq.from(errors),
          dependencies = ArraySeq.empty,
          activateWorkspace = None
        )
      )
  }

  /** Validate that the configured paths exist within the workspace directory */
  private def validatePathsExists(parsed: BuildState.Parsed)(implicit file: FileAccess): Option[BuildState.Errored] = {
    // absolute source paths
    val (_, absoluteContractPath, absoluteArtifactPath, absoluteDependenciesPath) =
      Build.getAbsolutePaths(parsed)

    val (contractPathIndex, artifactPathIndex, dependencyPathIndex) =
      Build.getPathIndexes(parsed)

    // do these paths exists with the workspace directory?
    val compileResult =
      for {
        contractExists     <- file.exists(absoluteContractPath.toUri, contractPathIndex)
        artifactsExists    <- file.existsOrUndefined(absoluteArtifactPath.map(_._2), artifactPathIndex)
        dependenciesExists <- dependencyPathExists(absoluteDependenciesPath, dependencyPathIndex)
      } yield (contractExists, artifactsExists, dependenciesExists)

    compileResult match {
      case Right((contractExists, artifactsExists, dependenciesExists)) =>
        val errors =
          ListBuffer.empty[CompilerMessage.AnyError]

        // check if contract path exists
        if (!contractExists)
          errors addOne
            ErrorDirectoryDoesNotExists(
              dirPath = parsed.config.contractPath,
              index = contractPathIndex
            )

        // check if artifact path exists
        if (!artifactsExists)
          absoluteArtifactPath foreach {
            case (artifactPath, _) =>
              errors addOne
                ErrorDirectoryDoesNotExists(
                  dirPath = artifactPath,
                  index = artifactPathIndex
                )
          }

        // check if dependencies path exists
        if (!dependenciesExists)
          parsed.config.dependencyPath foreach {
            dependencyPath =>
              errors addOne
                ErrorDirectoryDoesNotExists(
                  dirPath = dependencyPath,
                  index = dependencyPathIndex
                )
          }

        // check if errors exists
        if (errors.isEmpty) {
          None // No errors!
        } else {
          val errorState =
            BuildState.Errored( // report errors
              buildURI = parsed.buildURI,
              codeOption = Some(parsed.code),
              errors = ArraySeq.from(errors),
              dependencies = ArraySeq.empty,
              activateWorkspace = None
            )

          Some(errorState)
        }

      case Left(error) =>
        // exception occurred performing IO.
        val errors =
          BuildState.Errored(
            buildURI = parsed.buildURI,
            codeOption = Some(parsed.code),
            errors = ArraySeq(error),
            dependencies = ArraySeq.empty,
            activateWorkspace = None
          )

        Some(errors)
    }
  }

  /**
   * Checks if the configured `dependencyPath` exists.
   *
   * @param absoluteDependenciesPath The absolute dependency path.
   * @param dependencyPathIndex      The index in the source for reporting errors.
   * @return `true` if the `dependencyPath` exists or if it's the default value ([[Dependency.defaultPath]]), otherwise `false`.
   */
  private def dependencyPathExists(
      absoluteDependenciesPath: Option[Path],
      dependencyPathIndex: SourceIndex
    )(implicit file: FileAccess): Either[CompilerMessage.AnyError, Boolean] =
    absoluteDependenciesPath match {
      case Some(absoluteDependenciesPath) =>
        file
          .exists(absoluteDependenciesPath.toUri, dependencyPathIndex)
          .map {
            exists =>
              if (!exists)
                // The dependency path does not exist but allow using the default path,
                // which will be created and written to by the dependency compiler.
                Dependency
                  .defaultPath()
                  .exists {
                    defaultPath =>
                      defaultPath.resolve(absoluteDependenciesPath) == defaultPath
                  }
              else
                exists
          }

      case None =>
        // User did not define the dependencyPath.
        // Mark it as exists here, which defers the dependencies being written to the default dependencyPath via the dependency compiler.
        Right(true)
    }

}
