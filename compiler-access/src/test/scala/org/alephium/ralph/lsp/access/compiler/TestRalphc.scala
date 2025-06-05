// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler

import org.alephium.protocol.vm.StatefulContract
import org.alephium.ralph.{Warning, Ast, CompiledContract}
import org.alephium.util.AVector

/**
 * Test functions for generating data types returned by ralphc.
 */
object TestRalphc {

  def createTestContract(typeId: Ast.TypeId): Ast.Contract =
    Ast.Contract(
      stdIdEnabled = None,
      stdInterfaceId = None,
      isAbstract = false,
      ident = typeId,
      templateVars = Seq.empty,
      fields = Seq.empty,
      funcs = Seq.empty,
      maps = Seq.empty,
      events = Seq.empty,
      constantVars = Seq.empty,
      enums = Seq.empty,
      inheritances = Seq.empty,
      unitTests = Seq.empty
    )

  def createCompiledContract(
      typeId: Ast.TypeId,
      warnings: Warning*): CompiledContract =
    CompiledContract(
      code = StatefulContract(0, AVector.empty),
      ast = createTestContract(typeId),
      warnings = AVector.from(warnings),
      debugCode = StatefulContract(0, AVector.empty),
      tests = None
    )

  def createCompiledContract(
      ast: Ast.Contract,
      warnings: Warning*): CompiledContract =
    CompiledContract(
      code = StatefulContract(0, AVector.empty),
      ast = ast,
      warnings = AVector.from(warnings),
      debugCode = StatefulContract(0, AVector.empty),
      tests = None
    )

}
