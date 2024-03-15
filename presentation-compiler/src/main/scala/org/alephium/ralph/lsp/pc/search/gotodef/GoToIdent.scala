package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.access.compiler.ast.{AstExtra, Tree}
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension

import scala.collection.immutable.ArraySeq

private object GoToIdent {

  /**
   * Navigate to the argument(s) for the given identifier.
   *
   * @param identNode The node representing the identifier in the AST.
   * @param ident     The identifier for which the argument definition is sought.
   * @param source    The source tree to search within.
   * @return An array sequence of positioned ASTs matching the search result.
   */
  def goTo(identNode: Node[Ast.Positioned], ident: Ast.Ident, source: Tree.Source): ArraySeq[Ast.Positioned] =
    identNode.parent // take one step up to check the type of ident node.
      .to(ArraySeq)
      .collect {
        case variableNode @ Node(variable: Ast.Variable[_], _) if variable.id == ident => // Is it a variable?
          // The user clicked on a variable. Take 'em there!
          val arguments =
            goToArguments(
              variableNode = identNode,
              variable = variable,
              source = source
            )

          val constants =
            goToConstants(
              source = source,
              ident = ident
            )

          val localVariables =
            goToLocalVariables(
              childNode = variableNode,
              ident = ident
            )

          arguments ++ constants ++ localVariables

        case Node(fieldSelector: Ast.EnumFieldSelector[_], _) if fieldSelector.field == ident =>
          // The user clicked on an enum field. Take 'em there!
          goToEnumField(
            fieldSelector = fieldSelector,
            source = source
          )

        case node @ Node(field: Ast.EnumField, _) if field.ident == ident =>
          // The user clicked on an enum field.
          // Check the parent to find the enum type.
          node.parent
            .map(_.data)
            .to(ArraySeq)
            .collect {
              // Check: Parent is an enum definition which contains the enum field.
              case enumDef: Ast.EnumDef if enumDef.fields.exists(_.ident == field.ident) =>
                goToEnumFieldCalls(
                  enumType = enumDef.id,
                  enumField = field,
                  source = source
                )
            }
            .flatten

        case Node(constantDef: Ast.ConstantVarDef, _) if constantDef.ident == ident =>
          // The user clicked on an constant definition. Take 'em there!
          goToVariableUsages(
            ident = constantDef.ident,
            from = source.rootNode
          )

        case node @ Node(namedVar: Ast.NamedVar, _) if namedVar.ident == ident =>
          // User selected a named variable. Find its usages.
          goToNearestFuncDef(node).iterator
            .flatMap {
              case (functionNode, _) =>
                goToVariableUsages(
                  ident = namedVar.ident,
                  from = functionNode
                )
            }
      }
      .flatten

  /**
   * Navigate to the argument(s) for the given variable.
   *
   * @param variableNode The node representing the variable.
   * @param variable     The variable to find the argument for.
   * @param source       The source tree to search within.
   * @return An array sequence of [[Ast.Argument]]s matching the search result.
   */
  private def goToArguments(variableNode: Node[Ast.Positioned],
                            variable: Ast.Variable[_],
                            source: Tree.Source
                           ): Iterator[Ast.Argument] = {
    val functionArguments =
      goToNearestFunctionArguments(
        childNode = variableNode,
        ident = variable.id
      )

    val templateArguments =
      goToTemplateArguments(
        source = source,
        ident = variable.id
      )

    functionArguments ++ templateArguments
  }

  /**
   * Navigate to the constant definitions for the given identifier.
   *
   * @param source The source tree to search within.
   * @param ident  The constant identification to find.
   * @return The constant definitions.
   */
  private def goToConstants(source: Tree.Source, ident: Ast.Ident): Iterator[Ast.ConstantVarDef] =
    source.rootNode.walkDown
      .map(_.data)
      .collect {
        case constant: Ast.ConstantVarDef if constant.ident == ident =>
          constant
      }

  /**
   * Navigate to the local variable definitions within the function in scope.
   *
   * @param childNode The node within a function where the search starts.
   * @param ident     The identifier of the named variable to search for.
   * @return Variable definitions containing the named variable.
   */
  private def goToLocalVariables(childNode: Node[Ast.Positioned], ident: Ast.Ident): Iterator[Ast.VarDef[_]] =
    goToNearestFuncDef(childNode).iterator
      .flatMap {
        case (functionNode, _) =>
          functionNode.walkDown
            .collect {
              case Node(varDef: Ast.VarDef[_], _) if AstExtra.containsNamedVar(varDef, ident) =>
                varDef
            }
      }

  /**
   * Navigate to the enum field(s) for the given selected enum field.
   *
   * @param fieldSelector The selected enum field to find.
   * @param source        The source tree to search within.
   * @return An array sequence of [[Ast.EnumField]]s matching the search result.
   */
  private def goToEnumField(fieldSelector: Ast.EnumFieldSelector[_], source: Tree.Source): ArraySeq[Ast.EnumField] =
    source.ast match {
      case Left(contract: Ast.Contract) =>
        contract.enums
          .filter(_.id == fieldSelector.enumId)
          .flatMap(_.fields.find(_.ident == fieldSelector.field))
          .to(ArraySeq)

      case Left(_: Ast.ContractInterface | _: Ast.TxScript) | Right(_: Ast.Struct) =>
        ArraySeq.empty
    }

  /**
   * Navigate to all enum calls for the given enum type and field.
   *
   * @param enumType  The enum type to find.
   * @param enumField The enum field to find.
   * @param source    The source tree to search within.
   * @return An array sequence of enum field identities matching the search result.
   */
  private def goToEnumFieldCalls(enumType: Ast.TypeId,
                                 enumField: Ast.EnumField,
                                 source: Tree.Source
                                ): ArraySeq[Ast.Ident] =
    source.rootNode.walkDown
      .collect {
        // find all the selections matching the enum and the enum's field type.
        case Node(selector: Ast.EnumFieldSelector[_], _)
            if selector.enumId == enumType && selector.field == enumField.ident =>
          selector.field
      }
      .to(ArraySeq)

  /**
   * Navigate to all variable usages for the given variable identifier.
   *
   * @param ident The variable identifier to search for.
   * @param from  The node to search within, walking downwards.
   * @return An array sequence of variable usage IDs.
   */
  private def goToVariableUsages(ident: Ast.Ident, from: Node[Ast.Positioned]): ArraySeq[Ast.Ident] =
    from.walkDown
      .collect {
        // find all the selections matching the variable name.
        case Node(variable: Ast.Variable[_], _) if variable.id == ident =>
          variable.id
      }
      .to(ArraySeq)

  /**
   * Navigate to the nearest function definition for which the given child node is in scope.
   *
   * @param childNode The node to traverse up from.
   * @return An Option containing the nearest function definition, if found.
   */
  private def goToNearestFuncDef(childNode: Node[Ast.Positioned]): Option[(Node[Ast.Positioned], Ast.FuncDef[_])] =
    childNode.data.sourceIndex
      .flatMap {
        childNodeIndex =>
          childNode.walkParents
            .collectFirst {
              case node @ Node(function: Ast.FuncDef[_], _)
                  if function.sourceIndex.exists(_ contains childNodeIndex.index) =>
                (node, function)
            }
      }

  /**
   * Navigate to the argument(s) of the nearest function to this node.
   *
   * @param childNode The node to traverse up in search of the function.
   * @param ident     The variable identifier to find arguments for.
   * @return An array sequence of [[Ast.Argument]]s matching the search result.
   */
  private def goToNearestFunctionArguments(childNode: Node[Ast.Positioned], ident: Ast.Ident): Iterator[Ast.Argument] =
    goToNearestFuncDef(childNode).iterator
      .flatMap {
        case (_, funcDef) =>
          funcDef.args
            .filter(_.ident == ident)
      }

  /**
   * Navigate to the template argument(s) for the given identifier.
   *
   * @param source The source tree to search within.
   * @param ident  The variable identifier to find arguments for.
   * @return An array sequence of [[Ast.Argument]]s matching the search result.
   */
  private def goToTemplateArguments(source: Tree.Source, ident: Ast.Ident): Seq[Ast.Argument] = {
    val arguments =
      source.ast match {
        case Left(contract) =>
          contract match {
            case ast: Ast.TxScript =>
              ast.templateVars

            case contract: Ast.Contract =>
              contract.templateVars ++ contract.fields

            case _: Ast.ContractInterface =>
              Seq.empty
          }

        case Right(_) =>
          Seq.empty
      }

    arguments.filter(_.ident == ident)
  }

}
