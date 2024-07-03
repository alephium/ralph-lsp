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

import org.alephium.ralph._
import org.alephium.protocol.vm._

private object InconsistentWarningsFix {

  /*
   * The mutable state of `Ast.ContractWithState` is causing inconsistent warnings when compiling twice the same AST.
   * This function fixes the issue by performing a deep copy of the `Ast.ContractWithState` objects.
   * So the AST in `Tree.Source` is not mutated and we compile the copy of the ast.
   *
   * FIXME:
   * For some reason we still need to call `genStatefulContracts` to have all tests passing, some Go-To-Definition tests are failing.
   * But `genStatefulScripts` should not be called.
   */
  def fix(
      contracts: Seq[Ast.ContractWithState],
      structs: Seq[Ast.Struct],
      options: CompilerOptions): Seq[Ast.ContractWithState] = {
    // Mutate just what we need to have all go-to-definition working
    // TODO: We could remove this later if we find a better solution
    Ast.MultiContract(contracts, structs, None, None).extendedContracts().genStatefulContracts()(options)

    // Deep copy the AST to avoid inconsistent warnings
    contracts.map(copyContractWithState)
  }

  /**
   * All functions bellows are used to perform a deep copy of Ast.ContractState
   * As the original Ast.ContractState is mutable, we need to copy it to avoid side effects.
   */
  private def copyContractWithState(contract: Ast.ContractWithState): Ast.ContractWithState =
    contract match {
      case Ast.ContractInterface(stdId, useMethodSelector, ident, funcs, events, inheritances) =>
        new Ast.ContractInterface(stdId, useMethodSelector, ident, funcs.map(copyFuncDef), events, inheritances).atSourceIndex(contract.sourceIndex)
      case Ast.TxScript(ident, templateVars, funcs) =>
        new Ast.TxScript(ident, templateVars, funcs.map(copyFuncDef)).atSourceIndex(contract.sourceIndex)
      case Ast.Contract(stdIdEnabled, stdInterfaceId, isAbstract, ident, templateVars, fields, funcs, maps, events, constantVars, enums, inheritances) =>
        new Ast.Contract(
          stdIdEnabled,
          stdInterfaceId,
          isAbstract,
          ident,
          templateVars,
          fields,
          funcs.map(copyFuncDef),
          maps,
          events,
          constantVars.map(copyConstantVarDef),
          enums.map(copyEnumDef),
          inheritances
        ).atSourceIndex(contract.sourceIndex)
    }

  private def copyFuncDef[Ctx <: StatefulContext](funcDef: Ast.FuncDef[Ctx]) =
    new Ast.FuncDef(
      funcDef.annotations.map(copyAnnotation),
      funcDef.id,
      funcDef.isPublic,
      funcDef.usePreapprovedAssets,
      funcDef.useAssetsInContract,
      funcDef.usePayToContractOnly,
      funcDef.useCheckExternalCaller,
      funcDef.useUpdateFields,
      funcDef.useMethodIndex,
      funcDef.args,
      funcDef.rtypes,
      funcDef.bodyOpt.map(_.map(copyStatement))
    ).atSourceIndex(funcDef.sourceIndex)

  private def copyStatement[Ctx <: StatefulContext](statement: Ast.Statement[Ctx]): Ast.Statement[Ctx] =
    statement match {
      case Ast.InsertToMap(ident, args)          => new Ast.InsertToMap(ident, args.map(copyExpr)).atSourceIndex(statement.sourceIndex)
      case Ast.VarDef(vars, value)               => new Ast.VarDef(vars, copyExpr(value)).atSourceIndex(statement.sourceIndex)
      case Ast.StructDestruction(id, vars, expr) => new Ast.StructDestruction(id, vars, copyExpr(expr)).atSourceIndex(statement.sourceIndex)
      case Ast.EmitEvent(id, args)               => new Ast.EmitEvent(id, args.map(copyExpr)).atSourceIndex(statement.sourceIndex)
      case Ast.Assign(targets, rhs)              => new Ast.Assign(targets.map(copyAssignmentTarget), copyExpr(rhs)).atSourceIndex(statement.sourceIndex)
      case Ast.Debug(str, interpolParts)         => new Ast.Debug(str, interpolParts.map(copyExpr)).atSourceIndex(statement.sourceIndex)
      case Ast.IfElseStatement(ifBranches, elseBranchOpt) =>
        new Ast.IfElseStatement(ifBranches.map(copyIfBranchStatement), elseBranchOpt.map(copyElseStatement)).atSourceIndex(statement.sourceIndex)
      case Ast.ReturnStmt(exprs)      => new Ast.ReturnStmt(exprs.map(copyExpr)).atSourceIndex(statement.sourceIndex)
      case Ast.While(condition, body) => new Ast.While(copyExpr(condition), body.map(copyStatement)).atSourceIndex(statement.sourceIndex)
      case Ast.ContractCall(obj, callId, approveAssets, args) =>
        new Ast.ContractCall(copyExpr(obj), callId, approveAssets.map(copyApproveAsset), args.map(copyExpr)).atSourceIndex(statement.sourceIndex)
      case Ast.ForLoop(init, cond, update, body) =>
        new Ast.ForLoop(copyStatement(init), copyExpr(cond), copyStatement(update), body.map(copyStatement)).atSourceIndex(statement.sourceIndex)
      case Ast.FuncCall(id, approveAssets, args) => new Ast.FuncCall(id, approveAssets.map(copyApproveAsset), args.map(copyExpr)).atSourceIndex(statement.sourceIndex)
      case Ast.RemoveFromMap(ident, args)        => new Ast.RemoveFromMap(ident, args.map(copyExpr)).atSourceIndex(statement.sourceIndex)
      case Ast.StaticContractFuncCall(contractId, id, approveAssets, args) =>
        new Ast.StaticContractFuncCall(contractId, id, approveAssets.map(copyApproveAsset), args.map(copyExpr))
          .atSourceIndex(statement.sourceIndex)
          .atSourceIndex(statement.sourceIndex)
    }

  private def copyIfBranchStatement[Ctx <: StatefulContext](ifBranchStatement: Ast.IfBranchStatement[Ctx]): Ast.IfBranchStatement[Ctx] =
    new Ast.IfBranchStatement(
      copyExpr(ifBranchStatement.condition),
      ifBranchStatement.body.map(copyStatement)
    ).atSourceIndex(ifBranchStatement.sourceIndex)

  private def copyElseStatement[Ctx <: StatefulContext](elseBranchStatement: Ast.ElseBranchStatement[Ctx]): Ast.ElseBranchStatement[Ctx] =
    new Ast.ElseBranchStatement(
      elseBranchStatement.body.map(copyStatement)
    ).atSourceIndex(elseBranchStatement.sourceIndex)

  private def copyAssignmentTarget[Ctx <: StatefulContext](assignmentTarget: Ast.AssignmentTarget[Ctx]): Ast.AssignmentTarget[Ctx] =
    assignmentTarget match {
      case Ast.AssignmentSimpleTarget(ident)              => new Ast.AssignmentSimpleTarget(ident).atSourceIndex(assignmentTarget.sourceIndex)
      case Ast.AssignmentSelectedTarget(ident, selectors) => new Ast.AssignmentSelectedTarget(ident, selectors.map(copyDataSelector)).atSourceIndex(assignmentTarget.sourceIndex)
    }

  private def copyAnnotation[Ctx <: StatelessContext](annotation: Ast.Annotation[Ctx]): Ast.Annotation[Ctx] =
    new Ast.Annotation(
      annotation.id,
      annotation.fields.map(copyAnnotationField(_))
    ).atSourceIndex(annotation.sourceIndex)

  private def copyAnnotationField[Ctx <: StatelessContext](annotationField: Ast.AnnotationField[Ctx]): Ast.AnnotationField[Ctx] =
    new Ast.AnnotationField(
      annotationField.ident,
      copyConst(annotationField.value)
    ).atSourceIndex(annotationField.sourceIndex)

  private def copyConst[Ctx <: StatelessContext](const: Ast.Const[Ctx]): Ast.Const[Ctx] =
    new Ast.Const(const.v).atSourceIndex(const.sourceIndex)

  private def copyConstantVarDef[Ctx <: StatelessContext](constantVarDef: Ast.ConstantVarDef[Ctx]): Ast.ConstantVarDef[Ctx] =
    new Ast.ConstantVarDef(
      constantVarDef.ident,
      copyExpr(constantVarDef.expr)
    ).atSourceIndex(constantVarDef.sourceIndex)

  private def copyExpr[Ctx <: StatelessContext](expr: Ast.Expr[Ctx]): Ast.Expr[Ctx] =
    expr match {
      case Ast.CallExpr(id, approveAssets, args) =>
        new Ast.CallExpr(id, approveAssets.map(copyApproveAsset), args.map(copyExpr)).atSourceIndex(expr.sourceIndex)

      case Ast.Const(v) =>
        new Ast.Const(v).atSourceIndex(expr.sourceIndex)

      case Ast.ContractCallExpr(obj, callId, approveAssets, args) =>
        new Ast.ContractCallExpr(copyExpr(obj), callId, approveAssets.map(copyApproveAsset), args.map(copyExpr)).atSourceIndex(expr.sourceIndex)

      case Ast.ContractConv(contractType, address) =>
        new Ast.ContractConv(contractType, copyExpr(address)).atSourceIndex(expr.sourceIndex)

      case Ast.ContractStaticCallExpr(contractId, id, approveAssets, args) =>
        new Ast.ContractStaticCallExpr(contractId, id, approveAssets.map(copyApproveAsset), args.map(copyExpr)).atSourceIndex(expr.sourceIndex)

      case Ast.CreateArrayExpr(elements) =>
        new Ast.CreateArrayExpr(elements.map(copyExpr)).atSourceIndex(expr.sourceIndex)

      case Ast.EnumFieldSelector(enum, field) =>
        new Ast.EnumFieldSelector(enum, field).atSourceIndex(expr.sourceIndex)

      case Ast.IfElseExpr(ifBranches, elseBranch) =>
        new Ast.IfElseExpr(ifBranches.map(copyIfBranchExpr), copyElseBranchExpr(elseBranch)).atSourceIndex(expr.sourceIndex)

      case Ast.LoadDataBySelectors(base, selectors) =>
        new Ast.LoadDataBySelectors(copyExpr(base), selectors.map(copyDataSelector)).atSourceIndex(expr.sourceIndex)

      case Ast.MapContains(ident, index) =>
        new Ast.MapContains(ident, copyExpr(index)).atSourceIndex(expr.sourceIndex)

      case Ast.ParenExpr(expr) =>
        new Ast.ParenExpr(copyExpr(expr)).atSourceIndex(expr.sourceIndex)

      case Ast.StructCtor(id, fields) =>
        val newFields = fields.map {
          case (i, exprOpt) => (i, exprOpt.map(copyExpr))
        }
        new Ast.StructCtor(id, newFields).atSourceIndex(expr.sourceIndex)

      case Ast.ALPHTokenId() =>
        new Ast.ALPHTokenId().atSourceIndex(expr.sourceIndex)

      case Ast.Binop(op, left, right) =>
        new Ast.Binop(op, copyExpr(left), copyExpr(right)).atSourceIndex(expr.sourceIndex)

      case Ast.UnaryOp(op, expr) =>
        new Ast.UnaryOp(op, copyExpr(expr)).atSourceIndex(expr.sourceIndex)

      case Ast.Variable(ident) =>
        new Ast.Variable(ident).atSourceIndex(expr.sourceIndex)
    }

  private def copyIfBranchExpr[Ctx <: StatelessContext](ifBranchExpr: Ast.IfBranchExpr[Ctx]): Ast.IfBranchExpr[Ctx] =
    new Ast.IfBranchExpr(
      copyExpr(ifBranchExpr.condition),
      copyExpr(ifBranchExpr.expr)
    ).atSourceIndex(ifBranchExpr.sourceIndex)

  private def copyElseBranchExpr[Ctx <: StatelessContext](elseBranchExpr: Ast.ElseBranchExpr[Ctx]): Ast.ElseBranchExpr[Ctx] =
    new Ast.ElseBranchExpr(
      copyExpr(elseBranchExpr.expr)
    ).atSourceIndex(elseBranchExpr.sourceIndex)

  private def copyApproveAsset[Ctx <: StatelessContext](expr: Ast.ApproveAsset[Ctx]): Ast.ApproveAsset[Ctx] =
    Ast
      .ApproveAsset(copyExpr(expr.address),
                    expr.tokenAmounts.map {
                      case (l, r) => (copyExpr(l), copyExpr(r))
                    }
      )
      .atSourceIndex(expr.sourceIndex)

  private def copyEnumDef[Ctx <: StatelessContext](enumDef: Ast.EnumDef[Ctx]): Ast.EnumDef[Ctx] =
    new Ast.EnumDef(
      enumDef.id,
      enumDef.fields.map(copyEnumField)
    ).atSourceIndex(enumDef.sourceIndex)

  private def copyEnumField[Ctx <: StatelessContext](enumField: Ast.EnumField[Ctx]): Ast.EnumField[Ctx] =
    new Ast.EnumField(
      enumField.ident,
      copyConst(enumField.value)
    ).atSourceIndex(enumField.sourceIndex)

  private def copyDataSelector[Ctx <: StatelessContext](dataSelector: Ast.DataSelector): Ast.DataSelector =
    dataSelector match {
      case Ast.IndexSelector(index) =>
        new Ast.IndexSelector(copyExpr(index)).atSourceIndex(dataSelector.sourceIndex)
      case Ast.IdentSelector(ident) =>
        new Ast.IdentSelector(ident).atSourceIndex(dataSelector.sourceIndex)
    }

}
