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

package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.config.RalphcConfig
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorInvalidBuildSyntax
import org.alephium.ralph.lsp.pc.workspace.build.typescript.TestTSBuildFile
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._

/**
 * Test cases for [[Workspace.buildChanged]] function.
 */
class WorkspaceBuildChangedSpec extends AnyWordSpec with Matchers {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  "alephium.config.ts" should {
    "be processed, even if the current `ralph.json` contains errors" in {
      implicit val file: FileAccess         = FileAccess.disk
      implicit val compiler: CompilerAccess = CompilerAccess.ralphc

      val goodWorkspace =
        TestWorkspace
          .genCompiledOK()
          .sample
          .value

      // generate and persist `alephium.config.ts`
      val tsConfig       = TestTSBuildFile.genTSConfig.sample.value.copy(sourceDir = Some(""), artifactDir = Some(""))
      val tsConfigString = TestTSBuildFile.genTSBuildFile(tsConfig).sample.value
      TestFile.write(goodWorkspace.tsBuildURI, tsConfigString)

      // ralph.json should exist
      TestFile.exists(goodWorkspace.buildURI) shouldBe true
      // overwrite the content of ralph.json with bad code
      TestFile.write(goodWorkspace.buildURI, "blah")
      // execute build again so an error is returned because `blah` is not a valid code
      val erroredWorkspace =
        Workspace
          .build(
            code = None,
            workspace = WorkspaceState.Created(goodWorkspace.workspaceURI)
          )
          .left
          .value
          .error
          .value

      // errors exists
      erroredWorkspace.errors should have size 1
      erroredWorkspace.errors.head shouldBe a[ErrorInvalidBuildSyntax]

      // execute the buildChanged function, with both: the good workspace and the errored build
      val buildChangedWorkspace =
        Workspace
          .buildChanged(
            buildURI = goodWorkspace.tsBuildURI,
            code = None,
            workspace = goodWorkspace,
            buildErrors = Some(erroredWorkspace)
          )
          .value
          .value

      // `alephium.config.ts` resolves the workspace
      buildChangedWorkspace shouldBe a[WorkspaceState.Compiled]

      // The `ralph.json` on-disk is now parseable with valid syntax.
      val ralph_json        = TestFile.readAll(goodWorkspace.buildURI)
      val ralph_json_parsed = RalphcConfig.parse(goodWorkspace.buildURI, ralph_json).value
      // values from `alephium.config.ts` are copied into `ralph.json`
      ralph_json_parsed.contractPath shouldBe ""
      // Currently, artifactPath is not used until #84 is implemented, so its is always defaulted to None
      ralph_json_parsed.artifactPath shouldBe empty

      TestWorkspace delete goodWorkspace
    }
  }

}
