package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.build.BuildState._
import org.alephium.ralph.lsp.pc.workspace.build.error._

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer

/** Implements functions for validating `ralph.json` */
object BuildValidator {

  /** Validate and promotes a parsed build-file to compiled */
  def validate(parsed: BuildParsed)(implicit file: FileAccess): Option[BuildState.BuildErrored] =
    validatePathsInWorkspace(parsed) // Run validation checks
      .orElse(validatePathsExists(parsed))

  /** Checks that buildURI is in the project's root directory */
  def validateBuildURI(buildURI: URI,
                       workspaceURI: URI): Either[CompilerMessage.Error, URI] =
    if (!URIUtil.isFileName(buildURI, Build.BUILD_FILE_NAME))
      Left(ErrorBuildFileNotFound)
    else if (!URIUtil.isFirstChild(workspaceURI, buildURI)) // Build file must be in the root workspace directory.
      Left(
        ErrorInvalidBuildFileLocation(
          buildURI = buildURI,
          workspaceURI = workspaceURI
        )
      )
    else
      Right(buildURI)

  /** Validate that the configured paths are within the workspace directory */
  private def validatePathsInWorkspace(parsed: BuildParsed): Option[BuildState.BuildErrored] = {
    val contractPath = parsed.config.contractPath
    val artifactPath = parsed.config.artifactPath

    // absolute source paths
    val (workspacePath, absoluteContractPath, absoluteArtifactPath, _) =
      Build.getAbsolutePaths(parsed)

    val (contractPathIndex, artifactPathIndex, _) =
      Build.getPathIndexes(parsed)

    val errors =
      ListBuffer.empty[CompilerMessage.AnyError]

    // Validate: is the contract path within the workspace
    if (!URIUtil.contains(workspacePath, absoluteContractPath))
      errors addOne
        ErrorDirectoryOutsideWorkspace(
          dirPath = contractPath,
          index = contractPathIndex
        )

    // Validate: is the artifact path within the workspace
    if (!URIUtil.contains(workspacePath, absoluteArtifactPath))
      errors addOne
        ErrorDirectoryDoesNotExists(
          dirPath = artifactPath,
          index = artifactPathIndex
        )

    // Check if errors exists
    if (errors.isEmpty)
      None
    else
      Some(
        BuildErrored( // report errors
          buildURI = parsed.buildURI,
          code = Some(parsed.code),
          errors = ArraySeq.from(errors),
          dependency = None,
          activateWorkspace = None
        )
      )
  }

  /** Validate that the configured paths exist within the workspace directory */
  private def validatePathsExists(parsed: BuildParsed)(implicit file: FileAccess): Option[BuildState.BuildErrored] = {
    val contractPath = parsed.config.contractPath
    val artifactPath = parsed.config.artifactPath
    val dependencyPath = parsed.config.dependencyPath

    // absolute source paths
    val (_, absoluteContractPath, absoluteArtifactPath, absoluteDependenciesPath) =
      Build.getAbsolutePaths(parsed)

    val (contractPathIndex, artifactPathIndex, dependencyPathIndex) =
      Build.getPathIndexes(parsed)

    // do these paths exists with the workspace directory?
    val compileResult =
      for {
        contractExists <- file.exists(absoluteContractPath.toUri, contractPathIndex)
        artifactsExists <- file.exists(absoluteArtifactPath.toUri, artifactPathIndex)
        dependenciesExists <- file.exists(absoluteDependenciesPath.toUri, dependencyPathIndex)
      } yield (contractExists, artifactsExists, dependenciesExists)

    compileResult match {
      case Right((contractExists, artifactsExists, dependenciesExists)) =>
        val errors =
          ListBuffer.empty[CompilerMessage.AnyError]

        // check if contract path exists
        if (!contractExists)
          errors addOne
            ErrorDirectoryDoesNotExists(
              dirPath = contractPath,
              index = contractPathIndex
            )

        // check if artifact path exists
        if (!artifactsExists)
          errors addOne
            ErrorDirectoryDoesNotExists(
              dirPath = artifactPath,
              index = artifactPathIndex
            )

        // check if dependencies path exists
        if (!dependenciesExists)
          errors addOne
            ErrorDirectoryDoesNotExists(
              dirPath = dependencyPath,
              index = dependencyPathIndex
            )

        // check if errors exists
        if (errors.isEmpty) {
          None // No errors!
        } else {
          val errorState =
            BuildErrored( // report errors
              buildURI = parsed.buildURI,
              code = Some(parsed.code),
              errors = ArraySeq.from(errors),
              dependency = None,
              activateWorkspace = None
            )

          Some(errorState)
        }

      case Left(error) =>
        // exception occurred performing IO.
        val errors =
          BuildErrored(
            buildURI = parsed.buildURI,
            code = Some(parsed.code),
            errors = ArraySeq(error),
            dependency = None,
            activateWorkspace = None
          )

        Some(errors)
    }
  }

}
