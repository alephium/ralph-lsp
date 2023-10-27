package org.alephium.ralph.lsp.pc.util

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.pc.workspace.build.RalphcConfig.{RalphcCompiledConfig, RalphcParsedConfig}
import org.alephium.ralph.lsp.pc.completion.BuiltInFunction
import upickle.default._

import java.nio.file.{Path, Paths}

object PicklerUtil {

  implicit val pathReader: ReadWriter[Path] =
    readwriter[String].bimap(_.toString, Paths.get(_))

  implicit val compilerOptionsReaderWriter: ReadWriter[CompilerOptions] =
    macroRW

  implicit val ralphcConfigReaderWriter: ReadWriter[RalphcParsedConfig] =
    macroRW

  implicit val ralphcCompiledReaderWriter: ReadWriter[RalphcCompiledConfig] =
    macroRW

  implicit val builtInFunctionoReaderWriter: ReadWriter[BuiltInFunction] =
    macroRW
}
