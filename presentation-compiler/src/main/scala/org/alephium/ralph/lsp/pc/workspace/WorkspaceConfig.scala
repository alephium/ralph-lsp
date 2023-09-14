package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.pc.util.FileIO
import org.alephium.ralph.lsp.pc.util.PicklerUtil._
import org.alephium.ralphc.Config
import upickle.default._

import java.io.FileNotFoundException
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.util.{Failure, Try}

object WorkspaceConfig {

  /** The workspace config file name */
  val FILE_NAME = "ralphc-config.json"

  /** Default config */
  val defaultRalphcConfig =
    Config(
      compilerOptions = CompilerOptions.Default,
      contractPath = Paths.get("./contracts"),
      artifactPath = Paths.get("./artifacts")
    )

  // TODO: Possibly emit a sample config file in the error message so it can be copied
  //       or add the ability to generate one.
  def fileNotFoundException(): FileNotFoundException =
    new FileNotFoundException(s"Please create a root '${WorkspaceConfig.FILE_NAME}' file.")

  /** Reads [[Config]] from the workspace */
  def readRalphcConfig(workspaceURI: URI): Try[Config] = {
    def readConfigFile(uri: URI) =
      for {
        json <- FileIO.readAllLines(uri)
        config <- WorkspaceConfig.readConfig(json)
      } yield config

    val filePath =
      Paths.get(workspaceURI).resolve(FILE_NAME)

    FileIO.exists(filePath) flatMap {
      exists =>
        if (exists)
          readConfigFile(filePath.toUri)
        else
          Failure(fileNotFoundException())
    }
  }

  def readConfig(json: String): Try[Config] =
    Try(read[Config](json))

  def readWorkspaceConfig(workspaceURI: URI): Try[WorkspaceConfig] =
    readRalphcConfig(workspaceURI) map {
      ralphcConfig =>
        WorkspaceConfig(
          workspaceURI = workspaceURI,
          ralphcConfig = ralphcConfig
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

}

final case class WorkspaceConfig(workspaceURI: URI,
                                 ralphcConfig: Config) {
  def contractURI: URI =
    ralphcConfig.contractPath.toUri

  def artifactURI: URI =
    ralphcConfig.artifactPath.toUri
}
