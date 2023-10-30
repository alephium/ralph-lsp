package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.sourcecode.imports.StdInterface
import org.alephium.ralph.lsp.pc.workspace.build.error._
import org.alephium.ralph.lsp.pc.workspace.build.BuildState._

import java.net.URI
import java.nio.file.{Path, Paths}
import scala.collection.immutable.ArraySeq
import com.typesafe.scalalogging.StrictLogging

object Build extends StrictLogging{

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
  def validate(parsed: BuildState.ParseResult)(implicit file: FileAccess): BuildState.ValidationResult =
    parsed match {
      case errored: BuildErrored =>
        // there are parsing errors
        errored

      case parsed: BuildParsed =>
        // parse successful. Perform compilation!
       BuildValidator.validate(parsed)
    }

  /** Parse and compile from disk */
  def parseAndValidate(buildURI: URI)(implicit file: FileAccess): BuildState.ValidationResult =
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
          validate(parse(buildURI))
        else
          BuildErrored(
            buildURI = buildURI,
            code = None,
            errors = ArraySeq(ErrorBuildFileNotFound),
            activateWorkspace = None
          )
    }

  /** Parse and compile from memory */
  def parseAndValidate(buildURI: URI,
                      code: String)(implicit file: FileAccess): BuildState.ValidationResult = {
    // Code is already read. Parse and validate it.
    val parsed =
      parse(
        buildURI = buildURI,
        json = code
      )

    validate(parsed)
  }

  def parseAndCompile(buildURI: URI,
                      code: Option[String],
                      currentBuild:Option[BuildState.BuildCompiled])(implicit file: FileAccess): BuildState.CompileResult =
    (code match {
      case Some(code) =>
        parseAndValidate(
          buildURI = buildURI,
          code = code
        )

      case None =>
        // Code is not known. Parse and validate it from disk.
        parseAndValidate(buildURI)
    }) match {
      case validated:BuildState.BuildValidated => compile(validated, currentBuild)
      case error: BuildErrored => error
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
          currentBuild = Some(currentBuild)
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

  //Currently version of dependencies are fix by the build.sbt
  //When version can be configured, we need here to check which version changed to load them
  def compile(validated: BuildState.BuildValidated, previous: Option[BuildState.BuildCompiled]): BuildState.CompileResult =
    previous match {
      case Some(previous) if previous.validated.depsVersion == validated.depsVersion =>
        toBuildCompiled(validated, previous.dependencies)
      case _ =>
        buildDependencies(validated)
    }

  def toBuildCompiled(validated: BuildState.BuildValidated, deps: BuildDependencies): BuildState.BuildCompiled =
    BuildState.BuildCompiled(
      validated.buildURI,
      validated.code,
      validated.config,
      deps,
      validated
    )

  //Dependenciens are only load from disk, later we could imagine downloading them on the fly.
  def buildDependencies(validated: BuildState.BuildValidated): BuildState.CompileResult = {
    logger.debug("Loading dependencies from disk")
    StdInterface.buildStdInterfaces match {
      case Right(stdInterfaces) =>
        toBuildCompiled(validated, BuildDependencies(stdInterfaces))
      case Left(error) =>
        BuildErrored(
        buildURI = validated.buildURI,
        code = None,
        errors = ArraySeq(error),
        activateWorkspace = None
        )
    }
  }
}
