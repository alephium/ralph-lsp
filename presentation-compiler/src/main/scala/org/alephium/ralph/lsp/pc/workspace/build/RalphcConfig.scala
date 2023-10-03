package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.{CompilerOptions, SourceIndex}
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.pc.util.PicklerUtil._
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorInvalidBuildSyntax
import org.alephium.ralph.lsp.pc.workspace.build.WorkspaceBuild.toBuildPath
import org.alephium.ralphc.Config

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
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
                                artifactPath: String)

  /** Default parsed config */
  val defaultParsedConfig: RalphcParsedConfig =
    RalphcParsedConfig(
      compilerOptions = CompilerOptions.Default,
      contractPath = "contracts",
      artifactPath = "artifacts"
    )

  /** Default compiled config */
  val defaultCompiledConfig: RalphcCompiledConfig =
    Config(
      compilerOptions = defaultParsedConfig.compilerOptions,
      contractPath = Paths.get(defaultParsedConfig.contractPath),
      artifactPath = Paths.get(defaultParsedConfig.artifactPath)
    )

  def parse(buildURI: URI,
            json: String): Either[FormattableError, RalphcParsedConfig] =
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
            index = SourceIndex(0, 1),
            message = throwable.getMessage
          )

        Left(error)
    }

  /**
   * Only a compiled config ([[RalphcCompiledConfig]]) can be persisted.
   * */
  def write(compiledConfig: RalphcCompiledConfig): String =
    upickle.default.write[Config](compiledConfig)

  /**
   * Creates a config file.
   *
   * This can be used to generate a default config [[defaultParsedConfig]]
   * for the user in their IDE workspace.
   *
   * @param workspacePath  Workspace root path
   * @param compiledConfig RalphcConfig to generate
   * @return Create file's path
   */
  def persist(workspacePath: Path,
              compiledConfig: RalphcCompiledConfig): Try[Path] =
    Try {
      val bytes = RalphcConfig.write(compiledConfig).getBytes(StandardCharsets.UTF_8)
      val buildFilePath = toBuildPath(workspacePath)
      Files.write(buildFilePath, bytes)
    }

}