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

import org.alephium.ralph.lsp.GenExtension.GenExtensionsImplicits
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.TestSourceCode._
import org.alephium.ralph.lsp.pc.sourcecode.{TestSourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.build.TestBuild
import org.alephium.ralph.lsp.{TestCode, TestFile}
import org.scalacheck.Gen

import java.net.URI
import scala.collection.immutable.ArraySeq
import org.scalatest.matchers.should.Matchers._

/** [[Workspace]] related test functions */
object TestWorkspace {

  def genCreated(directory: Gen[URI] = TestFile.genFolderURI()): Gen[WorkspaceState.Created] =
    directory map WorkspaceState.Created.apply

  /**
   * Generate a [[org.alephium.ralph.lsp.pc.workspace.WorkspaceState.Created]] workspace with OnDisk source code.
   *
   * @param directory The workspace directory
   * @param persist   When true, persist workspace directory and source code.
   */
  def genCreatedWithSourceCode(
      directory: Gen[URI] = TestFile.genFolderURI(),
      persist: Boolean = false): Gen[(WorkspaceState.Created, List[SourceCodeState.OnDisk])] =
    for {
      workspace  <- TestWorkspace.genCreated(directory)
      sourceCode <- Gen.listOfMax()(TestSourceCode.genOnDiskForRoot(workspace.workspaceURI))
    } yield
      if (persist)
        (TestWorkspace.persist(workspace), TestSourceCode.persistAll(sourceCode))
      else
        (workspace, sourceCode)

  /**
   * Generates a parsed workspace from the provided source code.
   *
   * @param code The source code to parse.
   * @return A parsed workspace.
   */
  def genParsedOK(
      code: String*
    )(implicit compiler: CompilerAccess,
      file: FileAccess,
      client: ClientLogger): Gen[WorkspaceState.Parsed] =
    genParsed(code: _*).map(_.asInstanceOf[WorkspaceState.Parsed])

  /**
   * Generates a parsed or errored workspace from the provided source code.
   *
   * @param code The source code to parse.
   * @return The workspace containing the parser result.
   */
  def genParsed(
      code: String*
    )(implicit compiler: CompilerAccess,
      file: FileAccess,
      client: ClientLogger): Gen[WorkspaceState.IsParsed] =
    genUnCompiled(code: _*) map {
      unCompiled =>
        // try parsing the workspace
        val workspace =
          Workspace.parse(unCompiled)

        // it should contains the all the source file
        workspace.sourceCode.length shouldBe code.size

        workspace
    }

  /**
   * Generates an un-compiled workspace from the provided source code.
   *
   * @param code The source code to create an un-compiled workspace for.
   * @return An un-compiled workspace.
   */
  def genUnCompiled(
      code: String*
    )(implicit compiler: CompilerAccess,
      file: FileAccess,
      client: ClientLogger): Gen[WorkspaceState.UnCompiled] =
    // Generate a build file
    TestBuild.genCompiledOK() map {
      build =>
        val sourceFiles =
          code
            .zipWithIndex
            .map {
              case (code, index) =>
                SourceCodeState.UnCompiled(
                  fileURI = build.contractURI.resolve(s"file$index.ral"),
                  code = code
                )
            }
            .to(ArraySeq)

        // create a workspace for the source files above
        val unCompiled =
          WorkspaceState.UnCompiled(
            build = build,
            sourceCode = sourceFiles
          )

        // it should contains the all the source file
        unCompiled.sourceCode.length shouldBe code.size

        unCompiled
    }

  def persist[W <: WorkspaceState.IsSourceAware](
      workspace: W,
      code: Gen[String] = TestCode.genGoodCode()): W = {
    TestFile.createDirectories(workspace.workspaceURI)
    persistAll(
      sourceCode = workspace.sourceCode,
      code = code
    )
    workspace
  }

  def persist(workspace: WorkspaceState.Created): WorkspaceState.Created = {
    TestFile.createDirectories(workspace.workspaceURI)
    workspace
  }

  /** Delete the workspace folder and all its content */
  def delete[W <: WorkspaceState](workspace: W): W = {
    TestFile.deleteAll(workspace.workspaceURI)
    workspace
  }

}
