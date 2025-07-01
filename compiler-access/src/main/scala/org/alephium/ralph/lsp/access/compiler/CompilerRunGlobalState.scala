// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler

import org.alephium.protocol.vm.StatefulContext
import org.alephium.ralph.Ast

object CompilerRunGlobalState {

  def apply(state: Ast.GlobalState[StatefulContext]): CompilerRunGlobalState =
    new CompilerRunGlobalState(state)

}

/**
 * Exposes the APIs of ralphc returned global state ([[Ast.GlobalState]]) from a completed compiler run.
 */
class CompilerRunGlobalState private (globalState: Ast.GlobalState[StatefulContext]) {

  def getStruct(typeId: Ast.TypeId): Option[Ast.Struct] =
    globalState.tryGetStruct(typeId)

  /**
   * LSP does not assert the contents of [[Ast.GlobalState]]. This is managed by ralphc.
   * Therefore, in LSP, all [[CompilerRunGlobalState]] instances are considered equal.
   */
  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[CompilerRunGlobalState]

  override def hashCode(): Int =
    0

}
