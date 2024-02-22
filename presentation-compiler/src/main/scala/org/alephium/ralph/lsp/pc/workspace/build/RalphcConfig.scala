package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.pc.util.PicklerUtil._
import org.alephium.ralph.lsp.pc.workspace.build.Build.toBuildPath
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorInvalidBuildSyntax

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Try

object RalphcConfig {

  /**
   * Within the context of presentation-compiler:
   *  - Type [[RalphcCompiledConfig]] i.e. of type [[org.alephium.ralphc.Config]] serves as the compiled ralphc-config.
   *  - Type [[RalphcParsedConfig]] serves as the parsed ralphc-config.
   *
   * [[org.alephium.ralphc.Config]] is not used during parsing because string input for
   * fields such as `contractPath` and `artifactPath` lose the original user's input String
   * value after converting it to `Path` type, which in-case of errors displays incorrect error highlighting.
   * */
  type RalphcCompiledConfig =
    org.alephium.ralphc.Config

  case class RalphcParsedConfig(compilerOptions: CompilerOptions,
                                contractPath: String,
                                artifactPath: String,
                                dependencyPath: Option[String] = None)

  /** Default parsed config */
  val defaultParsedConfig: RalphcParsedConfig =
    RalphcParsedConfig(
      compilerOptions = CompilerOptions.Default,
      contractPath = "contracts",
      artifactPath = "artifacts",
      dependencyPath = None
    )

  def parse(buildURI: URI,
            json: String): Either[CompilerMessage.AnyError, RalphcParsedConfig] =
    try
      Right(upickle.default.read[RalphcParsedConfig](json))
    catch {
      case abortError: upickle.core.AbortException =>
        // Exact location of the error is known so build a FormattableError
        val error =
          ErrorInvalidBuildSyntax(
            buildURI = buildURI,
            error = abortError
          )

        Left(error)

      case parseError: ujson.ParseException =>
        // Exact location of the error is known so build a FormattableError
        val error =
          ErrorInvalidBuildSyntax(
            buildURI = buildURI,
            error = parseError
          )

        Left(error)

      case throwable: Throwable =>
        // The location of the error is unknown, report it
        // at the first character within the build file.
        val error =
          ErrorInvalidBuildSyntax(
            fileURI = buildURI,
            index = SourceIndex.empty,
            message = throwable.getMessage
          )

        Left(error)
    }

  /** Write a parsed config */
  def write(config: RalphcParsedConfig): String =
    upickle.default.write[RalphcParsedConfig](config)

  /** Write a compiled config */
  def write(config: RalphcCompiledConfig): String =
    upickle.default.write[RalphcCompiledConfig](config)

  /**
   * Creates a config file.
   *
   * This can be used to generate a default config [[defaultParsedConfig]]
   * for the user in their IDE workspace.
   *
   * @param workspacePath Workspace root path
   * @param config        Config to persist
   * @return Created file-path
   */
  def persist(workspacePath: Path,
              config: RalphcParsedConfig): Try[Path] =
    Try {
      val bytes = RalphcConfig.write(config).getBytes(StandardCharsets.UTF_8)
      val buildFilePath = toBuildPath(workspacePath)
      Files.write(buildFilePath, bytes)
    }

}
