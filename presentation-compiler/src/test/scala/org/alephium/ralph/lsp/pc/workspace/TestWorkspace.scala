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
import org.alephium.ralph.lsp.pc.sourcecode.TestSourceCode._
import org.alephium.ralph.lsp.pc.sourcecode.{TestSourceCode, SourceCodeState}
import org.alephium.ralph.lsp.{TestCode, TestFile}
import org.scalacheck.Gen

import java.net.URI

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
