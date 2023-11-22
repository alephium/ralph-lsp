package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.build.error._
import org.alephium.ralph.lsp.pc.workspace.build.BuildState._

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
                       workspaceURI: URI): Either[ErrorInvalidBuildFileLocation, URI] =
    if (URIUtil.isFirstChild(workspaceURI, buildURI)) // Build file must be in the root workspace directory.
      Right(buildURI)
    else
      Left(
        ErrorInvalidBuildFileLocation(
          buildURI = buildURI,
          workspaceURI = workspaceURI
        )
      )

  /** Validate that the configured paths are within the workspace directory */
  private def validatePathsInWorkspace(parsed: BuildParsed): Option[BuildState.BuildErrored] = {
    val contractPath = parsed.config.contractPath
    val artifactPath = parsed.config.artifactPath

    // absolute source paths
    val (workspacePath, absoluteContractPath, absoluteArtifactPath) =
      Build.getAbsolutePaths(parsed)

    val errors =
      ListBuffer.empty[CompilerMessage.AnyError]

    // Validate: is the contract path within the workspace
    if (!URIUtil.contains(workspacePath, absoluteContractPath))
      errors addOne
        ErrorDirectoryOutsideWorkspace(
          dirPath = contractPath,
          index =
            SourceIndex.ensurePositive(
              index = parsed.code.lastIndexOf(contractPath), // TODO: lastIndexOf is temporary solution until an AST is available.
              width = contractPath.length
            )
        )

    // Validate: is the artifact path within the workspace
    if (!URIUtil.contains(workspacePath, absoluteArtifactPath))
      errors addOne
        ErrorDirectoryDoesNotExists(
          dirPath = artifactPath,
          index =
            SourceIndex.ensurePositive(
              index = parsed.code.lastIndexOf(artifactPath), // TODO: lastIndexOf is temporary solution until an AST is available.
              width = artifactPath.length
            )
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

    // absolute source paths
    val (_, absoluteContractPath, absoluteArtifactPath) =
      Build.getAbsolutePaths(parsed)

    // do these paths exists with the workspace directory?
    val compileResult =
      for {
        contractExists <- file.exists(absoluteContractPath.toUri)
        artifactsExists <- file.exists(absoluteArtifactPath.toUri)
      } yield (contractExists, artifactsExists)

    compileResult match {
      case Right((contractExists, artifactsExists)) =>
        val errors =
          ListBuffer.empty[CompilerMessage.AnyError]

        // check if contract path exists
        if (!contractExists)
          errors addOne
            ErrorDirectoryDoesNotExists(
              dirPath = contractPath,
              index =
                SourceIndex.ensurePositive(
                  index = parsed.code.lastIndexOf(contractPath), // TODO: lastIndexOf is temporary solution until an AST is available.
                  width = contractPath.length
                )
            )

        // check if artifact path exists
        if (!artifactsExists)
          errors addOne
            ErrorDirectoryDoesNotExists(
              dirPath = artifactPath,
              index =
                SourceIndex.ensurePositive(
                  index = parsed.code.lastIndexOf(artifactPath), // TODO: lastIndexOf is temporary solution until an AST is available.
                  width = artifactPath.length
                )
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
