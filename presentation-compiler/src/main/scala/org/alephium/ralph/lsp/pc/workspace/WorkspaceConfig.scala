package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.pc.util.FileIO
import org.alephium.ralph.lsp.pc.util.PicklerUtil._
import org.alephium.ralphc.Config
import upickle.default._

import java.net.URI
import java.nio.file.Paths
import scala.util.Try

object WorkspaceConfig {

  val FILE_NAME = "ralphc-config.json"

  val defaultConfig =
    Config(
      compilerOptions = CompilerOptions.Default,
      contractPath = Paths.get("./contracts"),
      artifactPath = Paths.get("./artifacts")
    )

  def readConfig(workspaceURI: URI): Try[Config] =
    for {
      json <- FileIO.readAllLines(Paths.get(workspaceURI).resolve(FILE_NAME).toUri)
      config <- WorkspaceConfig.readConfig(json)
    } yield config

  def readConfig(json: String): Try[Config] =
    Try(read[Config](json))

  def readWorkspaceConfig(workspaceURI: URI): Try[WorkspaceConfig] =
    WorkspaceConfig.readConfig(workspaceURI) map {
      config =>
        WorkspaceConfig(
          workspaceURI = workspaceURI,
          config = config
        )
    }

  def writeConfig(config: Config): String =
    write[Config](config)

}

final case class WorkspaceConfig(workspaceURI: URI,
                                 config: Config)
