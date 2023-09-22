package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.compiler.error.StringMessage
import org.alephium.ralph.lsp.pc.util.FileIO
import org.alephium.ralph.lsp.pc.util.PicklerUtil._
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

  def parseConfig(json: String): Try[Config] =
    Try(read[Config](json))

  // TODO: Possibly emit a sample config file in the error message so it can be copied
  //       or add the ability to generate one.
  def buildNotFound(): String =
    s"Project build file not found. Create a '$BUILD_FILE_NAME' file in the project's root folder."

  def readConfigFile(buildURI: URI): BuildState =
    FileIO.readAllLines(buildURI) match {
      case Failure(exception) =>
        BuildErrored(
          buildURI = buildURI,
          code = None,
          errors = ArraySeq(StringMessage(exception.getMessage))
        )

      case Success(json) =>
        parseConfig(json) match {
          case Failure(exception) =>
            BuildErrored(
              buildURI = buildURI,
              code = Some(json),
              errors = ArraySeq(StringMessage(exception.getMessage))
            )

          case Success(config) =>
            BuildCompiled(
              buildURI = buildURI,
              code = json,
              config = config
            )
        }
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
          errors = ArraySeq(StringMessage(exception.getMessage))
        )

      case Success(exists) =>
        if (exists)
          readConfigFile(buildFilePath.toUri)
        else
          BuildErrored(
            buildURI = buildURI,
            code = None,
            errors = ArraySeq(StringMessage(buildNotFound()))
          )
    }
  }

  def readBuild(buildURI: URI,
                code: String): BuildState =
    parseConfig(code) match {
      case Failure(exception) =>
        // TODO: Error messages in the build file should report source-location.
        //       The JSON parser reports the errored index, which can be used here.
        BuildErrored(
          buildURI = buildURI,
          code = Some(code),
          errors = ArraySeq(StringMessage(exception.getMessage))
        )

      case Success(config) =>
        BuildCompiled(
          buildURI = buildURI,
          code = code,
          config = config
        )
    }

  def readBuild(buildURI: URI,
                code: Option[String]): BuildState =
    code match {
      case Some(buildJSON) =>
        // Code is already read. Parse and validate it.
        readBuild(
          buildURI = buildURI,
          code = buildJSON
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
                       workspaceURI: URI): Either[StringMessage, URI] =
    if (Paths.get(buildURI).getParent != Paths.get(workspaceURI)) // Build file must be in the root workspace folder.
      Left(StringMessage(s"Build file '$buildURI' does not belong to workspace '$workspaceURI'. It must be placed in the root folder"))
    else
      Right(buildURI)

}
