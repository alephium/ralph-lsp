package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.compiler.message.error.ThrowableError
import org.alephium.ralph.lsp.pc.util.FileIO
import org.alephium.ralph.lsp.pc.workspace.build.error._
import org.alephium.ralph.lsp.pc.workspace.build.BuildState._
import org.alephium.ralph.lsp.pc.sourcecode.imports.StdInterface

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
          errors = ArraySeq(ThrowableError(exception))
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
       buildDependencies(BuildValidator.validate(parsed))
    }

  /** Parse and compile from disk */
  def parseAndCompile(buildURI: URI): BuildState.Compiled =
    FileIO.exists(Paths.get(buildURI)) match {
      case Failure(exception) =>
        BuildErrored(
          buildURI = buildURI,
          code = None,
          errors = ArraySeq(ThrowableError(exception))
        )

      case Success(exists) =>
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
                      code: String): BuildState.Compiled = {
    // Code is already read. Parse and validate it.
    val parsed =
      parse(
        buildURI = buildURI,
        json = code
      )

    compile(parsed)
  }

  def parseAndCompile(buildURI: URI,
                      code: Option[String]): BuildState.Compiled =
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

  def buildDependencies(compiled: BuildState.Compiled): BuildState.Compiled =
    compiled match {
      case compiled: BuildCompiled =>
        StdInterface.buildStdInterfaces match {
          case Right(stdInterfaces) =>
            compiled.copy(dependencies = compiled.dependencies.copy(stdInterfaces = stdInterfaces))
          case Left(error) =>
              BuildErrored(
                buildURI = compiled.buildURI,
                code = None,
                errors = ArraySeq(error)
              )
        }
      case errored: BuildErrored =>
        errored
    }
}
