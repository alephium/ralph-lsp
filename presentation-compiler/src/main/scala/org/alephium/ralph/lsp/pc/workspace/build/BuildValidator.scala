package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.compiler.error.StringError
import org.alephium.ralph.lsp.pc.util.FileIO
import org.alephium.ralph.lsp.pc.workspace.build.error._
import org.alephium.ralph.lsp.pc.workspace.build.BuildState._
import org.alephium.ralph.SourceIndex
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralphc.Config

import java.net.URI
import java.nio.file.Paths
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

object BuildValidator {

  def validate(parsed: BuildParsed): BuildState.Compiled =
    BuildValidator.validDirectoryInWorkspace(parsed) match {
      case parsed: BuildParsed =>
        BuildValidator.validateDirectoryExists(parsed)

      case errored: BuildErrored =>
        errored
    }

  /** Checks that buildURI is in the project's root directory */
  def validateBuildURI(buildURI: URI,
                       workspaceURI: URI): Either[ErrorInvalidBuildFileLocation, URI] =
    if (Paths.get(buildURI).getParent != Paths.get(workspaceURI)) // Build file must be in the root workspace folder.
      Left(
        ErrorInvalidBuildFileLocation(
          buildURI = buildURI,
          workspaceURI = workspaceURI
        )
      )
    else
      Right(buildURI)

  /** Validate that the configured paths are within the workspace directory */
  private def validDirectoryInWorkspace(parsed: BuildParsed): BuildState.Parsed = {
    val workspacePath = Paths.get(parsed.workspaceURI)
    val contractPath = parsed.config.contractPath
    val artifactPath = parsed.config.artifactPath

    // absolute source paths
    val absoluteContractPath = workspacePath.resolve(contractPath)
    val absoluteArtifactPath = workspacePath.resolve(artifactPath)

    val errors =
      ListBuffer.empty[FormattableError]

    // Validate: is the contract path with the workspace
    if (!absoluteContractPath.startsWith(workspacePath))
      errors addOne
        ErrorDirectoryOutsideWorkspace(
          dirPath = contractPath,
          index =
            SourceIndex(
              // TODO: lastIndexOf is not ideal for all cases.
              index = parsed.code.lastIndexOf(contractPath) max 0,
              width = contractPath.length max 1
            )
        )

    // Validate: is the artifact path with the workspace
    if (!absoluteArtifactPath.startsWith(workspacePath))
      errors addOne
        ErrorDirectoryDoesNotExists(
          dirPath = artifactPath,
          index =
            SourceIndex(
              // TODO: lastIndexOf is not ideal for all cases.
              index = parsed.code.lastIndexOf(artifactPath) max 0,
              width = artifactPath.length max 1
            )
        )

    // Check if errors exists
    if (errors.isEmpty)
      parsed
    else
      BuildErrored( // report errors
        buildURI = parsed.buildURI,
        code = Some(parsed.code),
        errors = ArraySeq.from(errors)
      )
  }

  /** Validate that the configured paths exist within the workspace directory */
  private def validateDirectoryExists(parsed: BuildParsed): BuildState.Compiled = {
    val workspacePath = Paths.get(parsed.workspaceURI)
    val contractPath = parsed.config.contractPath
    val artifactPath = parsed.config.artifactPath

    // absolute source paths
    val absoluteContractPath = workspacePath.resolve(contractPath)
    val absoluteArtifactPath = workspacePath.resolve(artifactPath)

    // do these paths exists with the workspace directory?
    val compileResult =
      for {
        contractExists <- FileIO.exists(absoluteContractPath)
        artifactsExists <- FileIO.exists(absoluteArtifactPath)
      } yield (contractExists, artifactsExists)

    compileResult match {
      case Success((contractExists, artifactsExists)) =>
        val errors =
          ListBuffer.empty[FormattableError]

        // check if contract path exists
        if (!contractExists)
          errors addOne
            ErrorDirectoryDoesNotExists(
              dirPath = contractPath,
              index =
                SourceIndex(
                  // TODO: lastIndexOf is not ideal for all cases.
                  index = parsed.code.lastIndexOf(contractPath) max 0,
                  width = contractPath.length max 1
                )
            )

        // check if artifact path exists
        if (!artifactsExists)
          errors addOne
            ErrorDirectoryDoesNotExists(
              dirPath = artifactPath,
              index =
                SourceIndex(
                  // TODO: lastIndexOf is not ideal for all cases.
                  index = parsed.code.lastIndexOf(artifactPath) max 0,
                  width = artifactPath.length max 1
                )
            )

        // check if errors exists
        if (errors.isEmpty)
          BuildCompiled( // No errors! Convert to Compiled typed.
            buildURI = parsed.buildURI,
            code = parsed.code,
            config = Config(
              compilerOptions = parsed.config.compilerOptions,
              contractPath = absoluteContractPath,
              artifactPath = absoluteArtifactPath
            )
          )
        else
          BuildErrored( // report errors
            buildURI = parsed.buildURI,
            code = Some(parsed.code),
            errors = ArraySeq.from(errors)
          )

      case Failure(exception) =>
        // exception occurred performing IO.
        BuildErrored(
          buildURI = parsed.buildURI,
          code = Some(parsed.code),
          errors = ArraySeq(StringError(exception.getMessage))
        )
    }
  }

}
