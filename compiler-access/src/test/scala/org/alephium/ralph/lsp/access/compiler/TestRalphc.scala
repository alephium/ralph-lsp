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
      inheritances = Seq.empty
    )

  def createCompiledContract(
      typeId: Ast.TypeId,
      warnings: Warning*): CompiledContract =
    CompiledContract(
      code = StatefulContract(0, AVector.empty),
      ast = createTestContract(typeId),
      warnings = AVector.from(warnings),
      debugCode = StatefulContract(0, AVector.empty)
    )

  def createCompiledContract(
      ast: Ast.Contract,
      warnings: Warning*): CompiledContract =
    CompiledContract(
      code = StatefulContract(0, AVector.empty),
      ast = ast,
      warnings = AVector.from(warnings),
      debugCode = StatefulContract(0, AVector.empty)
    )

}
