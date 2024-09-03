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

package org.alephium.ralph.lsp.pc.workspace.build.config

import org.alephium.ralph.CompilerOptions

import java.net.URI
import java.nio.file.Path

/**
 * Within the context of presentation-compiler:
 *  - Type [[RalphcConfigState.Compiled]] i.e. a wrapper for type [[org.alephium.ralphc.Config]] serves as the compiled ralphc-config.
 *  - Type [[RalphcConfigState.Parsed]] serves as the parsed ralphc-config.
 *
 * [[org.alephium.ralphc.Config]] is not accessed directly during parsing because:
 *  - It's `contractPath` and `artifactPath` are of type `Path` which lose the original
 *    user's input String value after converting it to `Path` type, which in-case of errors
 *    displays incorrect error highlighting.
 *  - Issue #247 requires `artifactPath` to be optional, but in [[org.alephium.ralphc.Config]] it's non-optional.
 */
object RalphcConfigState {

  object Parsed {

    /** Default parsed config */
    val default: RalphcConfigState.Parsed =
      RalphcConfigState.Parsed(
        contractPath = "",
        artifactPath = None,
        dependencyPath = None,
        compilerOptions = None
      )

  }

  /**
   * An instance indicating a parsed Ralphc configuration.
   */
  case class Parsed(
      contractPath: String,
      artifactPath: Option[String] = None,
      dependencyPath: Option[String] = None,
      compilerOptions: Option[CompilerOptionsParsed] = None) {

    /**
     * Converts [[CompilerOptionsParsed]] instance to a [[CompilerOptions]] instance, with
     * undefined fields set from [[CompilerOptions.Default]].
     */
    def toRalphcCompilerOptions(): CompilerOptions =
      compilerOptions.map(_.toCompilerOptions()) getOrElse CompilerOptions.Default

  }

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
  case class Compiled(
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

}
