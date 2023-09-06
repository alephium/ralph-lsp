package org.alephium.ralph.lsp.pc.util

import org.alephium.ralph.CompilerOptions
import org.alephium.ralphc.Config
import upickle.default._

import java.nio.file.{Path, Paths}

object PicklerUtil {

  implicit val pathReader: ReadWriter[Path] =
    readwriter[String].bimap(_.toString, Paths.get(_))

  implicit val compilerOptionsReader: ReadWriter[CompilerOptions] =
    macroRW

  implicit val configReader: ReadWriter[Config] =
    macroRW

}
