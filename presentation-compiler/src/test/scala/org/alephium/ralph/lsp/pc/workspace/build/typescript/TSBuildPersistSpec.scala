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
import org.alephium.ralph.lsp.pc.workspace.build.RalphcConfig
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq
import scala.util.Random

class TSBuildPersistSpec extends AnyWordSpec with Matchers with MockFactory with ScalaCheckDrivenPropertyChecks {

  "not persist" when {
    "new config remains unchanged" when {
      "existing `ralph.json` content is in-memory" in {
        implicit val file: FileAccess = null // null implies there should be no IO.
        val jsonBuildURI              = TestFile.genFileURI().sample.get
        val tsBuildURI                = TestFile.genFileURI().sample.get
        val currentJSONConfig         = RalphcConfig.defaultParsedConfig

        // Randomly select indent because formatting of the JSON is irrelevant.
        val currentJSONBuildCode = RalphcConfig.write(currentJSONConfig, indent = Random.nextInt(10))
        val newConfig            = currentJSONConfig

        TSBuild
          .persist(
            jsonBuildURI = jsonBuildURI,
            tsBuildURI = tsBuildURI,
            tsBuildCode = None,
            jsonBuildCode = Some(currentJSONBuildCode),
            updatedConfig = newConfig
          )
          .value shouldBe empty
      }

      "existing `ralph.json` content not in-memory" in {
        val jsonBuildURI      = TestFile.genFileURI().sample.get
        val tsBuildURI        = TestFile.genFileURI().sample.get
        val currentJSONConfig = RalphcConfig.defaultParsedConfig

        // Randomly select indent because formatting of the JSON is irrelevant.
        val currentJSONBuildCode = RalphcConfig.write(currentJSONConfig, indent = Random.nextInt(10))
        val newConfig            = currentJSONConfig

        implicit val file: FileAccess = mock[FileAccess]
        // Only read is invoked. write should be invoked, indicating that JSON is not persisted.
        // This is important to check to ensure re-build does not occur unnecessarily.
        (file.read _)
          .expects(jsonBuildURI)
          .returns(Right(currentJSONBuildCode))
          .once()

        TSBuild
          .persist(
            jsonBuildURI = jsonBuildURI,
            tsBuildURI = tsBuildURI,
            tsBuildCode = None,
            jsonBuildCode = None, // None so that JSON is fetched from disk.
            updatedConfig = newConfig
          )
          .value shouldBe empty // empty! Not persisted!
      }
    }
  }

  "persist" when {
    "existing `ralph.json` is in error state" in {
      implicit val file: FileAccess = FileAccess.disk
      // Randomly select indent because formatting of the JSON is irrelevant.
      val errorJSON = RalphcConfig.write(RalphcConfig.defaultParsedConfig, indent = Random.nextInt(10)).drop(10)
      val newConfig = RalphcConfig.defaultParsedConfig

      forAll(TestFile.genFileURI(), TestFile.genFileURI(), Gen.option(errorJSON)) {
        case (jsonBuildURI, tsBuildURI, errorJSONOption) =>
          // write the errored JSON
          TestFile.write(jsonBuildURI, errorJSON)

          TSBuild
            .persist(
              jsonBuildURI = jsonBuildURI,
              tsBuildURI = tsBuildURI,
              tsBuildCode = None,
              jsonBuildCode = errorJSONOption, // Optional so it's either read from memory or disk.
              updatedConfig = newConfig
            )
            .value shouldBe Some(newConfig) // new config is persisted

          // Check that the persisted file has the new JSON
          TestFile.readAll(jsonBuildURI) shouldBe RalphcConfig.write(newConfig, 2)

          TestFile delete jsonBuildURI
      }

    }

    "existing `ralph.json` does not exist" in {
      implicit val file: FileAccess = FileAccess.disk
      val newConfig                 = RalphcConfig.defaultParsedConfig
      val tsBuildCode               = Some("tsCode")

      forAll(TestFile.genFileURI(), TestFile.genFileURI()) {
        case (jsonBuildURI, tsBuildURI) =>
          TSBuild
            .persist(
              jsonBuildURI = jsonBuildURI,
              jsonBuildCode = None,
              tsBuildURI = tsBuildURI,
              tsBuildCode = tsBuildCode,
              updatedConfig = newConfig
            )
            .value shouldBe Some(newConfig) // new config is persisted

          // Check that the persisted file has the new JSON
          TestFile.readAll(jsonBuildURI) shouldBe RalphcConfig.write(newConfig, 2)

          TestFile delete jsonBuildURI
      }

    }
  }

  "report IO error" when {
    "persisted" in {
      val error                = ThrowableError("Something went wrong", new Throwable(), SourceIndex.empty)
      val jsonBuildURI         = TestFile.genFileURI().sample.get
      val tsBuildURI           = TestFile.genFileURI().sample.get
      val currentJSONConfig    = RalphcConfig.defaultParsedConfig
      val currentJSONBuildCode = RalphcConfig.write(currentJSONConfig, indent = Random.nextInt(10))
      val tsBuildCode          = Some("tsCode")

      implicit val file: FileAccess =
        mock[FileAccess]

      // write fails
      (file.write _)
        .expects(*, *, *)
        .returns(Left(error)) // inject IO error writing the JSON
        .once()

      val actual =
        TSBuild
          .persist(
            jsonBuildURI = jsonBuildURI,
            jsonBuildCode = Some(currentJSONBuildCode),
            tsBuildURI = tsBuildURI,
            tsBuildCode = tsBuildCode,
            updatedConfig = RalphcConfig.defaultParsedConfig.copy(contractPath = "updated_contract_path")
          )

      val expected =
        TSBuildState.Errored(
          buildURI = tsBuildURI, // error is reported on tsBuildURI
          code = tsBuildCode,    // TS source-code is stored
          errors = ArraySeq(error)
        )

      actual.left.value shouldBe expected
    }
  }

}
