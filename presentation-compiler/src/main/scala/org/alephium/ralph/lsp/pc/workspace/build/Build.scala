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
          errors = ArraySeq(error),
          activateWorkspace = None
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
          errors = ArraySeq(error),
          activateWorkspace = None
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
          errors = ArraySeq(error),
          activateWorkspace = None
        )

      case Right(exists) =>
        if (exists)
          compile(parse(buildURI))
        else
          BuildErrored(
            buildURI = buildURI,
            code = None,
            errors = ArraySeq(ErrorBuildFileNotFound),
            activateWorkspace = None
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

  /**
   * Parse and re-compile the build file.
   * */
  def parseAndCompile(buildURI: URI,
                      code: Option[String],
                      currentBuild: BuildState.BuildCompiled)(implicit file: FileAccess): Option[BuildState.CompileResult] =
    BuildValidator.validateBuildURI(
      buildURI = buildURI,
      workspaceURI = currentBuild.workspaceURI
    ) match {
      case Left(error) =>
        val buildError =
          BuildState.BuildErrored(
            buildURI = buildURI,
            code = code,
            errors = ArraySeq(error),
            activateWorkspace = None
          )

        Some(buildError)

      case Right(buildURI) =>
        Build.parseAndCompile(
          buildURI = buildURI,
          code = code,
        ) match {
          case newBuild: BuildState.BuildCompiled =>
            // if the new build-file is the same as current build-file, return it as
            // no-state-changed, so that a new build does not unnecessarily gets triggered.
            if (currentBuild == newBuild)
              None
            else // else the build file has changed, return the new build.
              Some(newBuild)

          case errored: BuildState.BuildErrored =>
            Some(errored)
        }
    }

}
