package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.{FileIO, GenCommon}
import org.alephium.ralph.lsp.GenCommon._
import org.alephium.ralph.lsp.pc.sourcecode.{GenSourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.build.BuildDependencies
import org.alephium.ralph.lsp.pc.workspace.build.BuildState.BuildCompiled
import org.alephium.ralph.lsp.pc.workspace.build.RalphcConfig.RalphcCompiledConfig
import org.alephium.ralphc.Config
import org.scalacheck.{Arbitrary, Gen}
import org.alephium.ralph.lsp.pc.sourcecode.GenSourceCode._
import org.scalacheck.Gen

import java.net.URI

object GenWorkspace {

  def genCreated(directory: Gen[URI] = GenCommon.genFolderURI()): Gen[WorkspaceState.Created] =
    directory map WorkspaceState.Created

  def persist[W <: WorkspaceState.SourceAware](workspace: W,
                                               code: Gen[String] = genGoodCode()): W = {
    FileIO.createDirectories(workspace.workspaceURI)
    persistAll(
      sourceCode = workspace.sourceCode,
      code = code
    )
    workspace
  }

  def persist(workspace: WorkspaceState.Created): Unit =
    FileIO.createDirectories(workspace.workspaceURI)
}
