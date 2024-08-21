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
import java.io.File
import scala.io.Source

class TSBuildTransformerSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "transform" when {
    "new config remains unchanged" in new Fixture {

      files.foreach {
        case (i, file) =>
          val tsConfig = TSBuildTransformer.extractTSConfig(file)
          expected(i) shouldBe tsConfig
      }
    }
  }

  trait Fixture {

    val path   = getClass.getResource("/alephium.config.ts.d")
    val folder = new File(path.getPath)

    val files = folder
      .listFiles
      .toList
      .map {
        file =>
          val source = Source.fromFile(file)
          try
            (file.getName, source.mkString) // Convert the file content to a String
          finally source.close()            // Make sure to close the file resource
      }

    val expected = Map(
      "alephium.config.1.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = Some("contracts"),
        artifactDir = Some("artifacts"),
        ignoreUnusedConstantsWarnings = Some(true),
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = Some(true)
      ),
      "alephium.config.2.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.3.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.4.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.5.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = Some(true),
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = Some(false)
      ),
      "alephium.config.6.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = Some(true),
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = Some(true)
      ),
      "alephium.config.7.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.8.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.9.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = Some(true),
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = Some(true)
      ),
      "alephium.config.10.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = Some("./src/contracts"),
        ignoreUnusedConstantsWarnings = Some(true),
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = Some(true)
      ),
      "alephium.config.11.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.12.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = Some(true),
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = Some(true)
      ),
      "alephium.config.13.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.14.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.15.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.16.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.17.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.18.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = Some(true),
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = Some(true)
      ),
      "alephium.config.19.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.20.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = Some(false),
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = Some(true)
      ),
      "alephium.config.21.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.22.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.23.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.24.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.25.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.26.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.27.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.28.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.29.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.30.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.31.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.32.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.33.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.34.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      ),
      "alephium.config.35.ts" -> TSBuildTransformer.TSConfig(
        sourceDir = None,
        artifactDir = None,
        ignoreUnusedConstantsWarnings = None,
        ignoreUnusedVariablesWarnings = None,
        ignoreUnusedFieldsWarnings = None,
        ignoreUnusedPrivateFunctionsWarnings = None,
        ignoreUpdateFieldsCheckWarnings = None,
        ignoreCheckExternalCallerWarnings = None,
        errorOnWarnings = None
      )
    )
  }
}
