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

package org.alephium.ralph.lsp.pc.workspace.build.dependency

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, TestWorkspace}
import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, RalphcConfig}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader.{StdInterfaceDownloader, DependencyDownloader}
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState, TestBuild}
import org.scalatest.matchers.should.Matchers._
import org.scalatest.OptionValues._

import java.nio.file.{Path, Paths}
import scala.collection.immutable.ArraySeq

object TestDependency {

  /** Build the standard library */
  def buildStd(): BuildState.Compiled = {
    implicit val logger: ClientLogger =
      TestClientLogger

    implicit val file: FileAccess =
      null

    implicit val compiler: CompilerAccess =
      CompilerAccess.ralphc

    // create a default build file.
    val parsed =
      BuildState.Parsed(
        buildURI = Paths.get(Build.FILE_NAME).toUri,
        code = RalphcConfig.write(RalphcConfigState.Parsed.default),
        config = RalphcConfigState.Parsed.default
      )

    // build the std dependency
    val dependencyBuild =
      Dependency
        .compile(
          parsed = parsed,
          currentBuild = None,
          downloaders = ArraySeq(StdInterfaceDownloader)
        )
        .asInstanceOf[BuildState.Compiled]

    // only the `std` dependency should exist in the build
    dependencyBuild.dependencies should have size 1
    dependencyBuild.findDependency(DependencyID.Std) shouldBe defined
    URIUtil.getFileName(dependencyBuild.dependencies.head.workspaceURI) shouldBe DependencyID.Std.dirName

    // return the build (the build contains the std workspace)
    dependencyBuild
  }

  /**
   * Builds a custom dependency downloader for the given code.
   *
   * @param depId   The ID to assign to the dependency.
   * @param depCode The code to include within the dependency.
   * @return A downloader that generates an uncompiled dependency workspace.
   */
  def buildDependencyDownloader(
      depId: DependencyID,
      depCode: String
    )(implicit file: FileAccess,
      compiler: CompilerAccess): DependencyDownloader =
    new DependencyDownloader {
      override def dependencyID: DependencyID =
        depId

      protected override def _download(
          dependencyPath: Path,
          errorIndex: SourceIndex
        )(implicit logger: ClientLogger): Either[ArraySeq[CompilerMessage.AnyError], WorkspaceState.UnCompiled] = {
        // Generate a build file for this dependency
        val build =
          TestBuild
            .genCompiledOK(
              // the dependency's folder name should be the ID's name
              workspaceURI = dependencyPath.resolve(depId.dirName).toUri,
              // the dependency itself does not have other dependencies
              dependencyDownloaders = ArraySeq.empty
            )
            .sample
            .value

        // Generate a workspace from the build file.
        val dependencyWorkspace =
          TestWorkspace
            .genUnCompiled(
              build = build,
              code = Seq(depCode)
            )
            .sample
            .value

        Right(dependencyWorkspace)
      }
    }

}
