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

package org.alephium.ralph.lsp.access.compiler.ast

import org.alephium.protocol.vm.StatelessContext
import org.alephium.ralph.{Type, Ast}

object AstExtra {

  /**
   * The name of the function created by [[Ast.FuncDef.main]].
   */
  final val TX_SCRIPT_MAIN_FUNCTION_NAME = "main"

  /**
   * Checks if the variable definition [[Ast.VarDef]]
   * contains a named variable with the given identifier [[Ast.Ident]].
   *
   * @param varDef The variable definition to check.
   * @param ident  The identifier of the variable to search for.
   * @return True if the variable definition contains a named variable with the given identifier, false otherwise.
   */
  def containsNamedVar[A <: StatelessContext](
      varDef: Ast.VarDef[A],
      ident: Ast.Ident): Boolean =
    varDef
      .vars
      .exists {
        case varType: Ast.NamedVar =>
          varType.ident == ident

        case _ =>
          false
      }

  /**
   * Return the function signature for the given function definition.
   *
   * @param funcDef The function definition.
   * @return The function signature.
   */
  def funcSignature[A <: StatelessContext](funcDef: Ast.FuncDef[A]): Ast.Positioned =
    if (funcDef.bodyOpt.isEmpty)
      funcDef // show the entire function definition to display the function signature.
    else
      // The function contains a body so return just the function id.
      // FIXME: There is still a need to display just the function signature.
      //        At the moment there is no AST type that provides just the function signature.
      funcDef.id

  /**
   * Fetches the type identifier for a given type.
   *
   * @param tpe The type for which to fetch the identifier.
   * @return `Some(TypeId)` if the type has an associated [[Ast.TypeId]], otherwise [[None]].
   */
  def getTypeId(tpe: Type): Option[Ast.TypeId] =
    tpe match {
      case Type.NamedType(id) =>
        Some(id)

      case Type.Struct(id) =>
        Some(id)

      case Type.Contract(id) =>
        Some(id)

      case Type.Bool | Type.I256 | Type.U256 | Type.ByteVec | Type.Address | _: Type.FixedSizeArray | _: Type.Map | Type.Panic =>
        None

    }

  /**
   * Fetches the type identifier for a given global definition.
   *
   * @param ast The type for which to fetch the identifier.
   * @return `Some(TypeId)` if the type has an associated [[Ast.TypeId]], otherwise [[None]].
   */
  def getTypeId(ast: Ast.GlobalDefinition): Option[Ast.TypeId] =
    ast match {
      case ast: Ast.ContractWithState =>
        Some(ast.ident)

      case ast: Ast.Struct =>
        Some(ast.id)

      case ast: Ast.EnumDef[_] =>
        Some(ast.id)

      case asset: Ast.AssetScript =>
        Some(asset.ident)

      case _: Ast.ConstantVarDef[_] =>
        None
    }

}
