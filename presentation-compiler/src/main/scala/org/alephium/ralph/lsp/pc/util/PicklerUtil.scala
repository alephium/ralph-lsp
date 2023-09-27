package org.alephium.ralph.lsp.pc.util

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.pc.workspace.build.RalphcConfig.{RalphcCompiledConfig, RalphcParsedConfig}
import upickle.default._

import java.nio.file.{Path, Paths}

object PicklerUtil {

  implicit val pathReader: ReadWriter[Path] =
    readwriter[String].bimap(_.toString, Paths.get(_))

  implicit val compilerOptionsReaderWriter: ReadWriter[CompilerOptions] =
    macroRW

  /** A parsed config implements only a reader. This type cannot be persisted. */
  implicit val ralphcConfigReaderWriter: Reader[RalphcParsedConfig] =
    macroR

  /** A compiled config implements both reader and writer. This type can be persisted. */
  implicit val ralphcCompiledReaderWriter: ReadWriter[RalphcCompiledConfig] =
    macroRW

}
