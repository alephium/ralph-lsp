// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.ast

import org.alephium.protocol.vm.StatelessContext
import org.alephium.ralph.{Ast, Type}
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra

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
   * Checks if the `current` AST's position is before the `anchor` AST's position.
   *
   * @param current The AST whose position is being tested.
   * @param anchor  The AST with which the position of `current` is compared.
   * @return `true` if `current`'s position is before `anchor`'s position, `false` otherwise.
   */
  def isBehind(
      current: Ast.Positioned,
      anchor: Ast.Positioned): Boolean =
    SourceIndexExtra.isBehind(
      current = current.sourceIndex,
      anchor = anchor.sourceIndex
    )

  /**
   * Checks if the `current` AST's position is after the `anchor` AST's position.
   *
   * @param current The AST whose position is being tested.
   * @param anchor  The AST with which the position of `current` is compared.
   * @return `true` if `current`'s position is after `anchor`'s position, `false` otherwise.
   */
  def isAhead(
      current: Ast.Positioned,
      anchor: Ast.Positioned): Boolean =
    SourceIndexExtra.isAhead(
      current = current.sourceIndex,
      anchor = anchor.sourceIndex
    )

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

      case Type.Bool | Type.I256 | Type.U256 | Type.ByteVec | Type.Address | _: Type.FixedSizeArray | _: Type.Map | Type.Panic | _: Type.Tuple =>
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

  /**
   * Fetches the type or named identifier for a given global definition.
   *
   * @param ast The type or named identifier for which to fetch the identifier.
   * @return [[Ast.TypeId]] if the type has an associated [[Ast.TypeId]], otherwise [[Ast.Ident]].
   */
  def getIdentOrTypeId(ast: Ast.GlobalDefinition): Either[Ast.TypeId, Ast.Ident] =
    ast match {
      case ast: Ast.ContractWithState =>
        Left(ast.ident)

      case ast: Ast.Struct =>
        Left(ast.id)

      case ast: Ast.EnumDef[_] =>
        Left(ast.id)

      case asset: Ast.AssetScript =>
        Left(asset.ident)

      case ast: Ast.ConstantVarDef[_] =>
        Right(ast.ident)
    }

}
