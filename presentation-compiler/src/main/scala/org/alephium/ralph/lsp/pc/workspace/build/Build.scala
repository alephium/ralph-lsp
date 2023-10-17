package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.build.error._
import org.alephium.ralph.lsp.pc.workspace.build.BuildState._

import java.net.URI
import java.nio.file.{Path, Paths}
import scala.collection.immutable.ArraySeq

object Build {

  val BUILD_FILE_EXTENSION = "ralph"

  /** Build file of a workspace */
  val BUILD_FILE_NAME = s"build.$BUILD_FILE_EXTENSION"

  def toBuildPath(workspacePath: Path): Path =
    workspacePath.resolve(BUILD_FILE_NAME)

  def toBuildURI(workspaceURI: URI): URI =
    toBuildPath(Paths.get(workspaceURI)).toUri

  /** Parse a build that is in-memory */
  def parse(buildURI: URI,
            json: String): BuildState.ParseResult =
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
  def parse(buildURI: URI)(implicit file: FileAccess): BuildState.ParseResult =
    file.read(buildURI) match {
      case Left(error) =>
        BuildErrored(
          buildURI = buildURI,
          code = None,
          errors = ArraySeq(error)
        )

      case Right(json) =>
        parse(
          buildURI = buildURI,
          json = json
        )
    }

  /** Compile a parsed build */
  def compile(parsed: BuildState.ParseResult)(implicit file: FileAccess): BuildState.CompileResult =
    parsed match {
      case errored: BuildErrored =>
        // there are parsing errors
        errored

      case parsed: BuildParsed =>
        // parse successful. Perform compilation!
        BuildValidator.validate(parsed)
    }

  /** Parse and compile from disk */
  def parseAndCompile(buildURI: URI)(implicit file: FileAccess): BuildState.CompileResult =
    file.exists(buildURI) match {
      case Left(error) =>
        BuildErrored(
          buildURI = buildURI,
          code = None,
          errors = ArraySeq(error)
        )

      case Right(exists) =>
        if (exists)
          compile(parse(buildURI))
        else
          BuildErrored(
            buildURI = buildURI,
            code = None,
            errors = ArraySeq(ErrorBuildFileNotFound)
          )
    }

  /** Parse and compile from memory */
  def parseAndCompile(buildURI: URI,
                      code: String)(implicit file: FileAccess): BuildState.CompileResult = {
    // Code is already read. Parse and validate it.
    val parsed =
      parse(
        buildURI = buildURI,
        json = code
      )

    compile(parsed)
  }

  def parseAndCompile(buildURI: URI,
                      code: Option[String])(implicit file: FileAccess): BuildState.CompileResult =
    code match {
      case Some(code) =>
        parseAndCompile(
          buildURI = buildURI,
          code = code
        )

      case None =>
        // Code is not known. Parse and validate it from disk.
        parseAndCompile(buildURI)
    }

}
