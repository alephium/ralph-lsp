package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.{CompilerOptions, SourceIndex}
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.compiler.error.StringError
import org.alephium.ralph.lsp.pc.util.FileIO
import org.alephium.ralph.lsp.pc.util.PicklerUtil._
import org.alephium.ralph.lsp.pc.workspace.build.error.{ErrorBuildFileNotFound, ErrorInvalidBuildFileLocation, ErrorInvalidBuildSyntax}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState.{BuildCompiled, BuildErrored}
import org.alephium.ralphc.Config
import upickle.default._

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.collection.immutable.ArraySeq
import scala.util.{Failure, Success, Try}

object WorkspaceBuild {

  val BUILD_FILE_EXTENSION = "ralph"

  /** Build file of a workspace */
  val BUILD_FILE_NAME = s"build.$BUILD_FILE_EXTENSION"

  /** Default config */
  val defaultRalphcConfig =
    Config(
      compilerOptions = CompilerOptions.Default,
      contractPath = Paths.get("./contracts"),
      artifactPath = Paths.get("./artifacts")
    )

  def toBuildPath(workspacePath: Path): Path =
    workspacePath.resolve(BUILD_FILE_NAME)

  def toBuildURI(workspaceURI: URI): URI =
    toBuildPath(Paths.get(workspaceURI)).toUri

  def parseConfig(buildURI: URI,
                  json: String): Either[FormattableError, Config] =
    try
      Right(read[Config](json))
    catch {
      case abortError: upickle.core.AbortException =>
        // Exact location of the error is known so build a FormattableError
        val error =
          ErrorInvalidBuildSyntax(
            buildURI = buildURI,
            error = abortError
          )

        Left(error)

      case parseError: ujson.ParseException =>
        // Exact location of the error is known so build a FormattableError
        val error =
          ErrorInvalidBuildSyntax(
            buildURI = buildURI,
            error = parseError
          )

        Left(error)

      case throwable: Throwable =>
        // The location of the error is unknown, report it
        // at the first character within the build file.
        val error =
          ErrorInvalidBuildSyntax(
            fileURI = buildURI,
            index = SourceIndex(0, 1),
            message = throwable.getMessage
          )

        Left(error)
    }

  def parseBuild(buildURI: URI,
                 json: String): BuildState =
    parseConfig(
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
        BuildCompiled(
          buildURI = buildURI,
          code = json,
          config = config
        )
    }

  def readConfigFile(buildURI: URI): BuildState =
    FileIO.readAllLines(buildURI) match {
      case Failure(exception) =>
        BuildErrored(
          buildURI = buildURI,
          code = None,
          errors = ArraySeq(StringError(exception.getMessage))
        )

      case Success(json) =>
        parseBuild(
          buildURI = buildURI,
          json = json
        )
    }

  /** Reads [[Config]] from the workspace */
  def readBuild(buildURI: URI): BuildState = {
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
          readConfigFile(buildFilePath.toUri)
        else
          BuildErrored(
            buildURI = buildURI,
            code = None,
            errors = ArraySeq(ErrorBuildFileNotFound)
          )
    }
  }

  def readBuild(buildURI: URI,
                code: Option[String]): BuildState =
    code match {
      case Some(buildJSON) =>
        // Code is already read. Parse and validate it.
        parseBuild(
          buildURI = buildURI,
          json = buildJSON
        )

      case None =>
        // Code is not known. Parse and validate it from disk.
        readBuild(buildURI)
    }

  def writeConfig(config: Config): String =
    write[Config](config)

  /**
   * Creates a config file.
   *
   * This can be used to generate a default config [[defaultRalphcConfig]]
   * for the user in their IDE workspace.
   *
   * @param workspacePath Workspace root path
   * @param config        Config to generate
   * @return Create file's path
   */
  def persistConfig(workspacePath: Path,
                    config: Config): Try[Path] =
    Try {
      val bytes = writeConfig(config).getBytes(StandardCharsets.UTF_8)
      val buildFilePath = toBuildPath(workspacePath)
      Files.write(buildFilePath, bytes)
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
