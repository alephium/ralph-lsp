package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.{FileIO, GenCode, GenCommon}
import org.alephium.ralph.lsp.pc.sourcecode.{GenSourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.sourcecode.GenSourceCode._
import org.alephium.ralph.lsp.GenExtension.GenExtensionsImplicits
import org.scalacheck.Gen

import java.net.URI

object GenWorkspace {

  def genCreated(directory: Gen[URI] = GenCommon.genFolderURI()): Gen[WorkspaceState.Created] =
    directory map WorkspaceState.Created

  /**
   * Generate a [[org.alephium.ralph.lsp.pc.workspace.WorkspaceState.Created]] workspace with OnDisk source code.
   *
   * @param directory The workspace directory
   * @param persist   When true, persist workspace directory and source code.
   */
  def genCreatedWithSourceCode(directory: Gen[URI] = GenCommon.genFolderURI(),
                               persist: Boolean = false): Gen[(WorkspaceState.Created, List[SourceCodeState.OnDisk])] =
    for {
      workspace <- GenWorkspace.genCreated(directory)
      sourceCode <- Gen.listOfMax()(GenSourceCode.genOnDiskForRoot(workspace.workspaceURI))
    } yield
      if (persist)
        (GenWorkspace.persist(workspace), GenSourceCode.persistAll(sourceCode))
      else
        (workspace, sourceCode)

  def persist[W <: WorkspaceState.IsSourceAware](workspace: W,
                                                 code: Gen[String] = GenCode.genGoodCode()): W = {
    FileIO.createDirectories(workspace.workspaceURI)
    persistAll(
      sourceCode = workspace.sourceCode,
      code = code
    )
    workspace
  }

  def persist(workspace: WorkspaceState.Created): WorkspaceState.Created = {
    FileIO.createDirectories(workspace.workspaceURI)
    workspace
  }
}
