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

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.build.dependency.{DependencyID, TestDependency}
import org.alephium.ralph.lsp.{TestCode, TestFile}
import org.alephium.ralph.{CompiledScript, CompilerOptions, CompiledContract}
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

class SourceCodeCompileSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "compile" should {
    "return empty compiled source-code" when {
      "source-code and dependency are empty" in {
        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        val result =
          SourceCode
            .compile(
              sourceCode = ArraySeq.empty,
              dependency = ArraySeq.empty,
              compilerOptions = CompilerOptions.Default,
              workspaceErrorURI = TestFile.genFolderURI().sample.value
            )
            .value

        result shouldBe empty
      }

      "only source-code are empty" in {
        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        val dependencyBuild =
          TestDependency.buildStd()

        val result =
          SourceCode
            .compile(
              sourceCode = ArraySeq.empty,
              dependency = dependencyBuild.findDependency(DependencyID.Std).value.sourceCode,
              compilerOptions = CompilerOptions.Default,
              workspaceErrorURI = TestFile.genFolderURI().sample.value
            )
            .value

        result shouldBe empty
      }
    }

    "all source files" should {
      "have a SourceCodeState.Compiled entry" in {
        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        // No File IO should occur because all code is already in-memory.
        implicit val file: FileAccess =
          null

        // all source types
        val source =
          ArraySeq(
            TestCode.genContract("MyContract"),   // A Contract
            TestCode.genAbstract("MyAbstract"),   // An Abstract
            TestCode.genInterface("MyInterface"), // An Interface
            TestCode.genScript("MyScript")        // A Script
          )
            .map(TestSourceCode.genParsedOK(_))
            .map(_.sample.get)

        // Expected that all source files have a SourceCodeState.Compiled entry
        val result =
          SourceCode
            .compile(
              sourceCode = source,
              dependency = ArraySeq.empty,
              compilerOptions = CompilerOptions.Default,
              workspaceErrorURI = Paths.get(source.head.fileURI).getParent.toUri // workspace URI
            )
            .value
            .map(_.asInstanceOf[SourceCodeState.Compiled])

        result should have size source.size.toLong

        /** First: MyContract */
        result.head.fileURI shouldBe source.head.fileURI
        result.head.code shouldBe source.head.code
        result.head.parsed shouldBe source.head
        result.head.compiledCode should have size 1
        result.head.compiledCode.head.left.value shouldBe a[CompiledContract] // Contract is compiled

        /** Second: MyAbstract */
        result(1).fileURI shouldBe source(1).fileURI
        result(1).code shouldBe source(1).code
        result(1).parsed shouldBe source(1)
        result(1).compiledCode shouldBe empty // Abstract is not compiled

        /** Third: MyInterface */
        result(2).fileURI shouldBe source(2).fileURI
        result(2).code shouldBe source(2).code
        result(2).parsed shouldBe source(2)
        result(2).compiledCode shouldBe empty // Interface is not compiled

        /** Fourth: MyInterface */
        result(3).fileURI shouldBe source(3).fileURI
        result(3).code shouldBe source(3).code
        result(3).parsed shouldBe source(3)
        result(3).compiledCode should have size 1
        result(3).compiledCode.head.value shouldBe a[CompiledScript] // Script is compiled
      }
    }
  }

}
