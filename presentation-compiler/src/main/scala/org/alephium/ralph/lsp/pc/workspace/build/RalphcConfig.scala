// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}
import org.alephium.ralph.lsp.pc.util.PicklerUtil._
import org.alephium.ralph.lsp.pc.workspace.build.error.{ErrorInvalidBuildSyntax, ErrorEmptyBuildFile}

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Files}
import scala.util.Try

/**
 * Within the context of presentation-compiler:
 *  - Type [[RalphcCompiledConfig]] i.e. a wrapper for type [[org.alephium.ralphc.Config]] serves as the compiled ralphc-config.
 *  - Type [[RalphcParsedConfig]] serves as the parsed ralphc-config.
 *
 * [[org.alephium.ralphc.Config]] is not accessed directly during parsing because:
 *  - It's `contractPath` and `artifactPath` are of type `Path` which lose the original
 *    user's input String value after converting it to `Path` type, which in-case of errors
 *    displays incorrect error highlighting.
 *  - Issue #247 requires `artifactPath` to be optional, but in [[org.alephium.ralphc.Config]] it's non-optional.
 */
object RalphcConfig {

  /**
   * An instance indicating a compiled Ralphc configuration.
   *
   * Access to [[org.alephium.ralphc.Config]] is private to prevent direct access
   * to [[org.alephium.ralphc.Config.artifactPath]], which is non-optional.
   * Issue <a href="https://github.com/alephium/ralph-lsp/issues/247">#247</a>
   * requires it to be optional in the LSP.
   *
   * TODO: The instance of [[org.alephium.ralphc.Config]] is not needed by the compiler.
   *       The compiler only requires [[CompilerOptions]].
   *       Remove the usage of [[org.alephium.ralphc.Config]].
   *
   * @param isArtifactsPathDefinedInBuild Indicates if the artifactsPath was defined in the build.
   * @param config                        Private compiler configuration.
   */
  case class RalphcCompiledConfig(
      isArtifactsPathDefinedInBuild: Boolean,
      private val config: org.alephium.ralphc.Config) {

    def contractPath: Path =
      config.contractPath

    def contractURI: URI =
      contractPath.toUri

    /** No direct access to [[config.artifactPath]] in LSP */
    def artifactPath: Option[Path] =
      if (isArtifactsPathDefinedInBuild)
        Some(config.artifactPath)
      else
        None

    def artifactURI: Option[URI] =
      artifactPath.map(_.toUri)

    def compilerOptions: CompilerOptions =
      config.compilerOptions

  }

  /**
   * An instance indicating a parsed Ralphc configuration.
   */
  case class RalphcParsedConfig(
      compilerOptions: CompilerOptions,
      contractPath: String,
      artifactPath: Option[String] = None,
      dependencyPath: Option[String] = None)

  /** Default parsed config */
  val defaultParsedConfig: RalphcParsedConfig =
    RalphcParsedConfig(
      compilerOptions = CompilerOptions.Default,
      contractPath = "contracts",
      artifactPath = None,
      dependencyPath = None
    )

  def parse(
      buildURI: URI,
      json: String): Either[CompilerMessage.AnyError, RalphcParsedConfig] =
    if (json.isBlank)
      Left(ErrorEmptyBuildFile(buildURI))
    else
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
              index = SourceIndexExtra.zero(buildURI),
              message = throwable.getMessage
            )

          Left(error)
      }

  /** Write a parsed config */
  def write(
      config: RalphcParsedConfig,
      indent: Int = -1): String =
    upickle.default.write[RalphcParsedConfig](config, indent = indent)

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
  def persist(
      workspacePath: Path,
      config: RalphcParsedConfig): Try[Path] =
    Try {
      val bytes         = RalphcConfig.write(config).getBytes(StandardCharsets.UTF_8)
      val buildFilePath = Build.toBuildFile(workspacePath)
      Files.write(buildFilePath, bytes)
    }

}
