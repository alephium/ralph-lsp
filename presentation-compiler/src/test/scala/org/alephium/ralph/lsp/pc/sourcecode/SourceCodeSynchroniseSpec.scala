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

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.GenExtension.GenExtensionsImplicits
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.TestWorkspace
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq
import scala.util.Random

class SourceCodeSynchroniseSpec extends AnyWordSpec with Matchers with MockFactory with ScalaCheckDrivenPropertyChecks {

  "synchronise" should {
    "always return source-code within the workspace" in {
      val generator =
        for {
          // a workspace with source-code inside the workspace
          (workspace, sourceCodeInside) <- TestWorkspace.genCreatedWithSourceCode(persist = true)
          // source code outside the workspace
          sourceCodeOutside <- Gen.listOfMax()(TestSourceCode.genOnDiskAndPersist().map(_._1))
          // some or all of inside source-code
          someCodeInside <- Gen.someOf(sourceCodeInside)
          // some or all outside source-code
          someCodeOutside <- Gen.someOf(sourceCodeOutside)
        } yield (workspace, sourceCodeInside, sourceCodeOutside, someCodeInside, someCodeOutside)

      implicit val fileAccess: FileAccess =
        FileAccess.disk

      forAll(generator) {
        case (workspace, sourceCodeInside, sourceCodeOutside, someOfInside, someOfOutside) =>
          // random source-code i.e. some of inside and some of outside.
          val inputSourceCode =
            Random.shuffle(someOfInside ++ someOfOutside).to(ArraySeq)

          val result =
            SourceCode.synchronise(
              sourceDirectory = workspace.workspaceURI,
              sourceCode = inputSourceCode
            )

          // end result should ALWAYS contains all the source-code within the workspace
          result.value should contain theSameElementsAs sourceCodeInside

          // clear all generate files
          TestWorkspace.delete(workspace)
          sourceCodeOutside foreach TestSourceCode.delete
      }
    }
  }

}
