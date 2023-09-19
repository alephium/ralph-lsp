package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.compiler.error.StringMessage
import org.alephium.ralph.lsp.pc.util.FileIO
import org.alephium.ralph.lsp.pc.util.PicklerUtil._
import org.alephium.ralphc.Config
import upickle.default._

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.util.{Failure, Success, Try}

object WorkspaceBuild {

  val FILE_EXTENSION = "ralph"

  /** Build file of a workspace */
  val FILE_NAME = s"build.$FILE_EXTENSION"

  /** Default config */
  val defaultRalphcConfig =
    Config(
      compilerOptions = CompilerOptions.Default,
      contractPath = Paths.get("./contracts"),
      artifactPath = Paths.get("./artifacts")
    )

  def toBuildPath(workspacePath: Path): Path =
    workspacePath.resolve(FILE_NAME)

  def toBuildURI(workspaceURI: URI): URI =
    toBuildPath(Paths.get(workspaceURI)).toUri

  def parseConfig(json: String): Try[Config] =
    Try(read[Config](json))

  // TODO: Possibly emit a sample config file in the error message so it can be copied
  //       or add the ability to generate one.
  def buildNotFound(): String =
    s"Please create a root '$FILE_NAME' file."

  /** Reads [[Config]] from the workspace */
  def readBuild(buildURI: URI): Either[StringMessage, WorkspaceBuild] = {
    def readConfigFile(uri: URI) =
      for {
        json <- FileIO.readAllLines(uri)
        config <- parseConfig(json)
      } yield
        WorkspaceBuild(
          buildURI = buildURI,
          code = json,
          config = config
        )

    val buildFilePath =
      Paths.get(buildURI)

    FileIO.exists(buildFilePath) match {
      case Failure(exception) =>
        Left(StringMessage(exception.getMessage))

      case Success(exists) =>
        if (exists)
          readConfigFile(buildFilePath.toUri) match {
            case Failure(exception) =>
              Left(StringMessage(exception.getMessage))

            case Success(build) =>
              Right(build)
          }
        else
          Left(StringMessage(buildNotFound()))
    }
  }

  def readBuild(buildURI: URI,
                code: String): Either[StringMessage, WorkspaceBuild] =
    parseConfig(code) match {
      case Failure(exception) =>
        // TODO: Error messages in the build file should report source-location.
        //       The JSON parser reports the errored index, which can be used here.
        Left(StringMessage(exception.getMessage))

      case Success(config) =>
        val build =
          WorkspaceBuild(
            buildURI = buildURI,
            code = code,
            config = config
          )

        Right(build)
    }

  def readBuild(buildURI: URI,
                code: Option[String]): Either[StringMessage, WorkspaceBuild] =
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

}

final case class WorkspaceBuild(buildURI: URI,
                                code: String,
                                config: Config) {

  val workspaceURI: URI =
    Paths.get(buildURI).getParent.toUri

  def contractURI: URI =
    config.contractPath.toUri

  def artifactURI: URI =
    config.artifactPath.toUri
}
