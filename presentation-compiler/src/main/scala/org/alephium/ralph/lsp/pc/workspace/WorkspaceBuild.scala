package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.compiler.error.StringError
import org.alephium.ralph.lsp.pc.util.FileIO
import org.alephium.ralph.lsp.pc.util.PicklerUtil._
import org.alephium.ralphc.Config
import upickle.default._

import java.io.FileNotFoundException
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

  // TODO: Possibly emit a sample config file in the error message so it can be copied
  //       or add the ability to generate one.
  def buildNotFound(): String =
    s"Please create a root '$FILE_NAME' file."

  /** Reads [[Config]] from the workspace */
  def parseConfig(workspaceURI: URI): Try[Config] = {
    def readConfigFile(uri: URI) =
      for {
        json <- FileIO.readAllLines(uri)
        config <- parseConfig(json)
      } yield config

    val filePath =
      Paths.get(workspaceURI).resolve(FILE_NAME)

    FileIO.exists(filePath) flatMap {
      exists =>
        if (exists)
          readConfigFile(filePath.toUri)
        else
          Failure(new FileNotFoundException(buildNotFound()))
    }
  }

  def parseConfig(json: String): Try[Config] =
    Try(read[Config](json))

  def readBuild(workspaceURI: URI): Try[WorkspaceBuild] =
    parseConfig(workspaceURI) map {
      ralphcConfig =>
        WorkspaceBuild(
          workspaceURI = workspaceURI,
          config = ralphcConfig
        )
    }

  def writeConfig(config: Config): String =
    write[Config](config)

  /**
   * Creates a config file.
   *
   * This can be used to generate a default config [[defaultRalphcConfig]]
   * for the user in their IDE workspace.
   *
   * @param workspaceURI Workspace root path
   * @param config       Config to generate
   * @return Create file's path
   */
  def persistConfig(workspaceURI: Path,
                    config: Config): Try[Path] =
    Try {
      val bytes = writeConfig(config).getBytes(StandardCharsets.UTF_8)
      val filePath = workspaceURI.resolve(FILE_NAME)
      Files.write(filePath, bytes)
    }

  def buildChanged(fileURI: URI,
                   build: Option[String]): Either[FormattableError, WorkspaceBuild] =
    build match {
      case Some(config) =>
        parseConfig(config) match {
          case Failure(exception) =>
            // TODO: Error messages in the build file should report source-location.
            Left(StringError(exception.getMessage))

          case Success(config) =>
            Right(WorkspaceBuild(fileURI, config))
        }

      case None =>
        // TODO: Error messages in the build file should report source-location.
        Left(StringError(buildNotFound()))
    }

}

final case class WorkspaceBuild(workspaceURI: URI,
                                config: Config) {
  def contractURI: URI =
    config.contractPath.toUri

  def artifactURI: URI =
    config.artifactPath.toUri
}
