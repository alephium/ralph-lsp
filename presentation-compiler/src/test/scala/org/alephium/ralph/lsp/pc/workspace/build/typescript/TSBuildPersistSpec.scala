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

package org.alephium.ralph.lsp.pc.workspace.build.typescript

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.message.error.ThrowableError
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, RalphcConfig}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq

class TSBuildPersistSpec extends AnyWordSpec with Matchers with MockFactory with ScalaCheckDrivenPropertyChecks {

  "not persist" when {
    "new config remains unchanged" in {
      implicit val file: FileAccess = null // null implies there should be no IO.
      val jsonBuildURI              = TestFile.genFileURI().sample.get
      val tsBuildURI                = TestFile.genFileURI().sample.get
      val config                    = RalphcConfigState.Parsed.default

      val actual =
        TSBuild.persist(
          jsonBuildURI = jsonBuildURI,
          currentConfig = Some(config),
          tsBuildURI = tsBuildURI,
          tsBuildCode = "TypeScript Code",
          updatedConfig = config
        )

      actual.value shouldBe empty
    }
  }

  "persist" when {
    "existing `ralph.json` is in error state or does not exist (currentConfig = None)" in {
      implicit val file: FileAccess = FileAccess.disk
      val newConfig                 = RalphcConfigState.Parsed.default
      val newConfigContent          = RalphcConfig.write(newConfig, indent = 2)

      forAll(TestFile.genFileURI(), TestFile.genFileURI()) {
        case (jsonBuildURI, tsBuildURI) =>
          val actual =
            TSBuild.persist(
              jsonBuildURI = jsonBuildURI,
              currentConfig = None, // None indicates error or does not exist
              tsBuildURI = tsBuildURI,
              tsBuildCode = "TypeScript Code",
              updatedConfig = newConfig
            )

          // expect is the persisted config's content
          val expected =
            BuildState.Parsed(
              buildURI = jsonBuildURI,
              code = newConfigContent,
              config = newConfig
            )

          actual.value shouldBe Some(expected) // new config is persisted

          // Check that the persisted file has the new JSON
          TestFile.readAll(jsonBuildURI) shouldBe newConfigContent

          TestFile delete jsonBuildURI
      }
    }

    "report IO error" when {
      "persisted" in {
        val error             = ThrowableError("Something went wrong", new Throwable(), SourceIndex.empty)
        val jsonBuildURI      = TestFile.genFileURI().sample.get
        val tsBuildURI        = TestFile.genFileURI().sample.get
        val currentJSONConfig = RalphcConfigState.Parsed.default
        val tsBuildCode       = "TypeScript Code"

        implicit val file: FileAccess =
          mock[FileAccess]

        // write fails
        (file.write _)
          .expects(*, *, *)
          .returns(Left(error)) // inject IO error writing the JSON
          .once()

        val actual =
          TSBuild.persist(
            jsonBuildURI = jsonBuildURI,
            currentConfig = Some(currentJSONConfig),
            tsBuildURI = tsBuildURI,
            tsBuildCode = tsBuildCode,
            // updated the config to differ from existing JSON, so persistence occurs.
            updatedConfig = RalphcConfigState.Parsed.default.copy(contractPath = "updated_contract_path")
          )

        val expected =
          TSBuildState.Errored(
            buildURI = tsBuildURI,    // error is reported on tsBuildURI
            code = Some(tsBuildCode), // TS source-code is stored
            errors = ArraySeq(error)
          )

        actual.left.value shouldBe expected
      }
    }

  }

}
