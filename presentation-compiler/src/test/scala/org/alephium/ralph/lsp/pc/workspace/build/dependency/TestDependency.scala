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

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, RalphcConfig}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader.StdInterfaceDownloader
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState}
import org.scalatest.matchers.should.Matchers._

import java.nio.file.Paths
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

}
