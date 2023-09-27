package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.compiler.error.StringError
import org.alephium.ralph.lsp.pc.util.FileIO
import org.alephium.ralph.lsp.pc.workspace.build.error._
import org.alephium.ralph.lsp.pc.workspace.build.BuildState._
import org.alephium.ralphc.Config

import java.net.URI
import java.nio.file.{Path, Paths}
import scala.collection.immutable.ArraySeq
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
        BuildValidator.validDirectoryInWorkspace(parsed) match {
          case compiled: BuildCompiled =>
            BuildValidator.validateDirectoryExists(compiled)

          case errored: BuildErrored =>
            errored
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
