package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.GenExtension.GenExtensionsImplicits
import org.alephium.ralph.lsp.pc.sourcecode.TestSourceCode._
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, TestSourceCode}
import org.alephium.ralph.lsp.{TestCode, TestFile}
import org.scalacheck.Gen

import java.net.URI

/** [[Workspace]] related test functions */
object TestWorkspace {

  def genCreated(directory: Gen[URI] = TestFile.genFolderURI()): Gen[WorkspaceState.Created] =
    directory map WorkspaceState.Created

  /**
   * Generate a [[org.alephium.ralph.lsp.pc.workspace.WorkspaceState.Created]] workspace with OnDisk source code.
   *
   * @param directory The workspace directory
   * @param persist   When true, persist workspace directory and source code.
   */
  def genCreatedWithSourceCode(directory: Gen[URI] = TestFile.genFolderURI(),
                               persist: Boolean = false
                              ): Gen[(WorkspaceState.Created, List[SourceCodeState.OnDisk])] =
    for {
      workspace <- TestWorkspace.genCreated(directory)
      sourceCode <- Gen.listOfMax()(TestSourceCode.genOnDiskForRoot(workspace.workspaceURI))
    } yield
      if (persist)
        (TestWorkspace.persist(workspace), TestSourceCode.persistAll(sourceCode))
      else
        (workspace, sourceCode)

  def persist[W <: WorkspaceState.IsSourceAware](workspace: W, code: Gen[String] = TestCode.genGoodCode()): W = {
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
