package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.compiler.error.StringError
import org.alephium.ralph.lsp.pc.util.FileIO
import org.alephium.ralph.lsp.pc.workspace.build.error._
import org.alephium.ralph.lsp.pc.workspace.build.BuildState._
import org.alephium.ralph.SourceIndex
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralphc.Config

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

object BuildValidator {

  /** Validate that the configured paths are within the workspace directory */
  def validDirectoryInWorkspace(parsed: BuildParsed): BuildState.Compiled = {
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
              index = parsed.code.lastIndexOf(contractPath),
              width = contractPath.length
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
              index = parsed.code.lastIndexOf(artifactPath),
              width = artifactPath.length
            )
        )

    // Check if errors exists
    if (errors.isEmpty)
      BuildCompiled( // No errors! Convert to Compiled typed.
        buildURI = parsed.buildURI,
        code = parsed.code,
        config = Config(
          compilerOptions = parsed.config.compilerOptions,
          contractPath = Paths.get(parsed.config.contractPath),
          artifactPath = Paths.get(parsed.config.artifactPath)
        )
      )
    else
      BuildErrored( // report errors
        buildURI = parsed.buildURI,
        code = Some(parsed.code),
        errors = ArraySeq.from(errors)
      )
  }

  /** Validate that the configured paths exist within the workspace directory */
  def validateDirectoryExists(compiled: BuildCompiled): BuildState.Compiled = {
    val contractPath = compiled.config.contractPath
    val contractPathString = contractPath.toString

    val artifactPath = compiled.config.artifactPath
    val artifactPathString = artifactPath.toString
    // do these paths exists with the workspace directory?
    val compileResult =
      for {
        contractExists <- FileIO.exists(contractPath)
        artifactsExists <- FileIO.exists(artifactPath)
      } yield (contractExists, artifactsExists)

    compileResult match {
      case Success((contractExists, artifactsExists)) =>
        val errors =
          ListBuffer.empty[FormattableError]

        // check if contract path exists
        if (!contractExists)
          errors addOne
            ErrorDirectoryDoesNotExists(
              dirPath = contractPathString,
              index =
                SourceIndex(
                  // TODO: lastIndexOf is not ideal for all cases.
                  index = compiled.code.lastIndexOf(contractPathString),
                  width = contractPathString.length
                )
            )

        // check if artifact path exists
        if (!artifactsExists)
          errors addOne
            ErrorDirectoryDoesNotExists(
              dirPath = artifactPathString,
              index =
                SourceIndex(
                  // TODO: lastIndexOf is not ideal for all cases.
                  index = compiled.code.lastIndexOf(artifactPathString),
                  width = artifactPathString.length
                )
            )

        // check if errors exists
        if (errors.isEmpty)
          compiled
        else
          BuildErrored( // report errors
            buildURI = compiled.buildURI,
            code = Some(compiled.code),
            errors = ArraySeq.from(errors)
          )

      case Failure(exception) =>
        // exception occurred performing IO.
        BuildErrored(
          buildURI = compiled.buildURI,
          code = Some(compiled.code),
          errors = ArraySeq(StringError(exception.getMessage))
        )
    }
  }

}
