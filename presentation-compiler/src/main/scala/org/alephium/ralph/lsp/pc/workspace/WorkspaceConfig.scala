package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.pc.util.FileIO
import org.alephium.ralph.lsp.pc.util.PicklerUtil._
import org.alephium.ralphc.Config
import upickle.default._

import java.io.FileNotFoundException
import java.net.URI
import java.nio.file.Paths
import scala.util.{Failure, Try}

object WorkspaceConfig {

  val FILE_NAME = "ralphc-config.json"

  val defaultRalphcConfig =
    Config(
      compilerOptions = CompilerOptions.Default,
      contractPath = Paths.get("./contracts"),
      artifactPath = Paths.get("./artifacts")
    )

  // TODO: Possibly emit a sample config file in the error message so it can be copied
  //       or add the ability to generate one.
  def errorNoConfigFile(): FileNotFoundException =
    new FileNotFoundException(s"Please configure a root '${WorkspaceConfig.FILE_NAME}' file.")

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
          Failure(errorNoConfigFile())
    }
  }

  def readConfig(json: String): Try[Config] =
    Try(read[Config](json))

  def readWorkspaceConfig(workspaceURI: URI): Try[WorkspaceConfig] =
    WorkspaceConfig.readRalphcConfig(workspaceURI) map {
      ralphcConfig =>
        WorkspaceConfig(
          workspaceURI = workspaceURI,
          ralphcConfig = ralphcConfig
        )
    }

  def writeConfig(config: Config): String =
    write[Config](config)

}

final case class WorkspaceConfig(workspaceURI: URI,
                                 ralphcConfig: Config) {
  def contractURI: URI =
    ralphcConfig.contractPath.toUri

  def artifactURI: URI =
    ralphcConfig.artifactPath.toUri
}
