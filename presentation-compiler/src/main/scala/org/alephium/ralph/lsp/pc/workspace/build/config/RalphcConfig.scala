// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace.build.config

import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}
import org.alephium.ralph.lsp.pc.util.PicklerUtil._
import org.alephium.ralph.lsp.pc.workspace.build.Build
import org.alephium.ralph.lsp.pc.workspace.build.error.{ErrorInvalidBuildSyntax, ErrorEmptyBuildFile}

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Files}
import scala.util.Try

object RalphcConfig {

  def parse(
      buildURI: URI,
      json: String): Either[CompilerMessage.AnyError, RalphcConfigState.Parsed] =
    if (json.isBlank)
      Left(ErrorEmptyBuildFile(buildURI))
    else
      try
        Right(upickle.default.read[RalphcConfigState.Parsed](json))
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
      config: RalphcConfigState.Parsed,
      indent: Int = -1): String =
    upickle.default.write[RalphcConfigState.Parsed](config, indent = indent)

  /**
   * Creates a config file.
   *
   * This can be used to generate a default config [[RalphcConfigState.Parsed.default]]
   * for the user in their IDE workspace.
   *
   * @param workspacePath Workspace root path
   * @param config        Config to persist
   * @return Created file-path
   */
  def persist(
      workspacePath: Path,
      config: RalphcConfigState.Parsed): Try[Path] =
    Try {
      val bytes         = RalphcConfig.write(config).getBytes(StandardCharsets.UTF_8)
      val buildFilePath = Build.toBuildFile(workspacePath)
      Files.write(buildFilePath, bytes)
    }

}
