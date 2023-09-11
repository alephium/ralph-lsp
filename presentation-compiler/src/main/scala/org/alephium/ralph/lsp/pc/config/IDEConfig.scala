package org.alephium.ralph.lsp.pc.config

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.pc.util.FileIO
import org.alephium.ralph.lsp.pc.util.PicklerUtil._
import org.alephium.ralphc.Config
import upickle.default._

import java.net.URI
import java.nio.file.Paths
import scala.util.Try

object IDEConfig {

  val FILE_NAME = "ralph-ide.json"

  val defaultConfig =
    Config(
      compilerOptions = CompilerOptions.Default,
      contractPath = Paths.get("./contracts"),
      artifactPath = Paths.get("./artifacts")
    )

  def readConfig(workspaceURI: URI): Try[Config] =
    for {
      json <- FileIO.readAllLines(Paths.get(workspaceURI).resolve(FILE_NAME).toUri)
      config <- IDEConfig.readConfig(json)
    } yield config

  def readConfig(json: String): Try[Config] =
    Try(read[Config](json))

  def readIDEConfig(workspaceURI: URI): Try[IDEConfig] =
    IDEConfig.readConfig(workspaceURI) map {
      config =>
        IDEConfig(
          workspaceURI = workspaceURI,
          config = config
        )
    }

  def writeConfig(config: Config): String =
    write[Config](config)

}

final case class IDEConfig(workspaceURI: URI,
                           config: Config)
