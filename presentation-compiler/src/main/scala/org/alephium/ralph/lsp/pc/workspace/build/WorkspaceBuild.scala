package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.compiler.error.StringError
import org.alephium.ralph.lsp.pc.util.FileIO
import org.alephium.ralph.lsp.pc.workspace.build.error._
import org.alephium.ralph.lsp.pc.workspace.build.BuildState._
import org.alephium.ralph.SourceIndex
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralphc.Config

import java.net.URI
import java.nio.file.{Path, Paths}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

object WorkspaceBuild {

  val BUILD_FILE_EXTENSION = "ralph"

  /** Build file of a workspace */
  val BUILD_FILE_NAME = s"build.$BUILD_FILE_EXTENSION"

  def toBuildPath(workspacePath: Path): Path =
    workspacePath.resolve(BUILD_FILE_NAME)

  def toBuildURI(workspaceURI: URI): URI =
    toBuildPath(Paths.get(workspaceURI)).toUri

  /** Parse a build that is in-memory */
  def parse(buildURI: URI,
            json: String): BuildState.Parsed =
    RalphcConfig.parse(
      buildURI = buildURI,
      json = json
    ) match {
      case Left(error) =>
        BuildErrored(
          buildURI = buildURI,
          code = Some(json),
          errors = ArraySeq(error)
        )

      case Right(config) =>
        BuildParsed(
          buildURI = buildURI,
          code = json,
          config = config
        )
    }

  /** Parse a build that is on-disk */
  def parse(buildURI: URI): BuildState.Parsed =
    FileIO.readAllLines(buildURI) match {
      case Failure(exception) =>
        BuildErrored(
          buildURI = buildURI,
          code = None,
          errors = ArraySeq(StringError(exception.getMessage))
        )

      case Success(json) =>
        parse(
          buildURI = buildURI,
          json = json
        )
    }

  /** Compile a parsed build */
  def compile(parsed: BuildState.Parsed): BuildState.Compiled =
    parsed match {
      case errored: BuildErrored =>
        // there are parsing errors
        errored

      case parsed: BuildParsed =>
        // parse successful. Perform compilation!
        validDirectoryInWorkspace(parsed) match {
          case compiled: BuildCompiled =>
            validateDirectoryExists(compiled)

          case errored: BuildErrored =>
            errored
        }
    }

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

  /** Reads [[Config]] from the workspace */
  def parseAndCompile(buildURI: URI): BuildState.Compiled = {
    val buildFilePath =
      Paths.get(buildURI)

    FileIO.exists(buildFilePath) match {
      case Failure(exception) =>
        BuildErrored(
          buildURI = buildURI,
          code = None,
          errors = ArraySeq(StringError(exception.getMessage))
        )

      case Success(exists) =>
        if (exists)
          compile(parse(buildFilePath.toUri))
        else
          BuildErrored(
            buildURI = buildURI,
            code = None,
            errors = ArraySeq(ErrorBuildFileNotFound)
          )
    }
  }

  def parseAndCompile(buildURI: URI,
                      code: Option[String]): BuildState.Compiled =
    code match {
      case Some(buildJSON) =>
        // Code is already read. Parse and validate it.
        val parsed =
          parse(
            buildURI = buildURI,
            json = buildJSON
          )

        compile(parsed)

      case None =>
        // Code is not known. Parse and validate it from disk.
        parseAndCompile(buildURI)
    }

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

}
