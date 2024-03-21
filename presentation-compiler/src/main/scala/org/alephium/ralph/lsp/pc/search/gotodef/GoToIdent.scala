package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.access.compiler.ast.{AstExtra, Tree}
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._

import scala.collection.immutable.ArraySeq

private object GoToIdent {

  /**
   * Navigate to the argument(s) for the given identifier.
   *
   * @param identNode The node representing the identifier in the AST.
   * @param ident     The identifier for which the argument definition is sought.
   * @param source    The source tree to search within.
   * @return An array sequence of positioned ASTs matching the search result.
   * */
  def goTo(identNode: Node[Ast.Positioned],
           ident: Ast.Ident,
           source: Tree.Source): ArraySeq[Ast.Positioned] =
    identNode
      .parent // take one step up to check the type of ident node.
      .to(ArraySeq)
      .collect {
        case variableNode @ Node(variable: Ast.Variable[_], _) if variable.id == ident =>
          // They selected a variable. Take 'em there!
          goToScopeDefinitions(
            identNode = variableNode,
            ident = variable.id,
            source = source
          )

        case assignmentNode @ Node(assignment: Ast.AssignmentSimpleTarget[_], _) if assignment.ident == ident =>
          // They selected an assignment. Take 'em there!
          goToScopeDefinitions(
            identNode = assignmentNode,
            ident = assignment.ident,
            source = source
          )

        case Node(fieldSelector: Ast.EnumFieldSelector[_], _) if fieldSelector.field == ident =>
          // They selected an enum field. Take 'em there!
          goToEnumField(
            fieldSelector = fieldSelector,
            source = source
          )

        case node @ Node(field: Ast.EnumField, _) if field.ident == ident =>
          // They selected an enum field.
          // Check the parent to find the enum type.
          node
            .parent
            .map(_.data)
            .to(ArraySeq)
            .collect {
              // Check: Parent is an enum definition which contains the enum field.
              case enumDef: Ast.EnumDef if enumDef.fields.exists(_.ident == field.ident) =>
                goToEnumFieldUsage(
                  enumType = enumDef.id,
                  enumField = field,
                  source = source
                )
            }
            .flatten

        case node @ Node(field: Ast.EventField, _) if field.ident == ident =>
          // They selected an event field.
          // Check the parent to find the event definition.
          node
            .parent
            .map(_.data)
            .to(ArraySeq)
            .collect {
              // Check: Parent is an enum definition which contains the enum field.
              case eventDef: Ast.EventDef if eventDef.fields.exists(_.ident == field.ident) =>
                goToEventFieldUsage(
                  eventDefId = eventDef.id,
                  eventFieldIndex = eventDef.fields.indexWhere(_.ident == field.ident),
                  source = source
                )
            }
            .flatten

        case constantDefNode @ Node(constantDef: Ast.ConstantVarDef, _) if constantDef.ident == ident =>
          // They selected a constant definition. Take 'em there!
          goToIdentUsage(
            fromNode = constantDefNode,
            fromNodeIdent = constantDef.ident,
            source = source
          )

        case namedVarNode @ Node(namedVar: Ast.NamedVar, _) if namedVar.ident == ident =>
          // User selected a named variable. Find its usages.
          goToIdentUsage(
            fromNode = namedVarNode,
            fromNodeIdent = namedVar.ident,
            source = source
          )

        case argumentNode @ Node(argument: Ast.Argument, _) if argument.ident == ident =>
          // They selected an argument. Take 'em there!
          goToIdentUsage(
            fromNode = argumentNode,
            fromNodeIdent = argument.ident,
            source = source
          )
      }
      .flatten

  /**
   * Navigate to variable usages of the given identity.
   *
   * @param fromNode      The node representing the parent of the selected identity.
   * @param fromNodeIdent The identity for which usages are to be found.
   * @param source        The source tree to search within.
   * @return An iterator containing identities representing the usage locations of input identity.
   */
  private def goToIdentUsage(fromNode: Node[Ast.Positioned],
                             fromNodeIdent: Ast.Ident,
                             source: Tree.Source): Iterator[Ast.Ident] =
    goToNearestBlockInScope(fromNode, source)
      .iterator
      .flatMap {
        from =>
          goToVariableUsages(
            ident = fromNodeIdent,
            from = from
          )
      }

  /**
   * Navigate to arguments, constants and variables for the given identity ([[Ast.Ident]]).
   *
   * @param identNode The node representing the identity.
   * @param ident     The identity data ([[Ast.Ident]]) within the node.
   * @param source    The source tree to search within.
   * @return An array sequence of arguments, constants and variables with the input identity.
   */
  private def goToScopeDefinitions(identNode: Node[Ast.Positioned],
                                   ident: Ast.Ident,
                                   source: Tree.Source): Iterator[Ast.Positioned] = {
    val arguments =
      goToArguments(
        variableNode = identNode,
        variableId = ident,
        source = source
      )

    val constants =
      goToConstants(
        source = source,
        ident = ident
      )

    val localVariables =
      goToInScopeVariables(
        childNode = identNode,
        ident = ident,
        source = source
      )

    arguments ++ constants ++ localVariables
  }

  /**
   * Navigate to the argument(s) for the given variable.
   *
   * @param variableNode The node representing the variable.
   * @param variableId   The variable to find the argument for.
   * @param source       The source tree to search within.
   * @return An array sequence of [[Ast.Argument]]s matching the search result.
   * */
  private def goToArguments(variableNode: Node[Ast.Positioned],
                            variableId: Ast.Ident,
                            source: Tree.Source): Iterator[Ast.Argument] = {
    val functionArguments =
      goToNearestFunctionArguments(
        childNode = variableNode,
        ident = variableId
      )

    val templateArguments =
      goToTemplateArguments(
        source = source,
        ident = variableId
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
  private def goToConstants(source: Tree.Source,
                            ident: Ast.Ident): Iterator[Ast.ConstantVarDef] =
    source
      .rootNode
      .walkDown
      .map(_.data)
      .collect {
        case constant: Ast.ConstantVarDef if constant.ident == ident =>
          constant
      }

  /**
   * Navigate to the variable definitions within its scope.
   *
   * @param childNode The node within a function where the search starts.
   * @param ident     The identifier of the named variable to search for.
   * @return Variable definitions containing the named variable.
   */
  private def goToInScopeVariables(childNode: Node[Ast.Positioned],
                                   ident: Ast.Ident,
                                   source: Tree.Source): Iterator[Ast.VarDef[_]] =
    goToNearestBlockInScope(childNode, source)
      .iterator
      .flatMap {
        block =>
          block
            .walkDown
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
   * */
  private def goToEnumField(fieldSelector: Ast.EnumFieldSelector[_],
                            source: Tree.Source): ArraySeq[Ast.EnumField] =
    source.ast match {
      case Left(contract: Ast.Contract) =>
        contract
          .enums
          .filter(_.id == fieldSelector.enumId)
          .flatMap(_.fields.find(_.ident == fieldSelector.field))
          .to(ArraySeq)

      case Left(_: Ast.ContractInterface | _: Ast.TxScript) | Right(_: Ast.Struct) =>
        ArraySeq.empty
    }

  /**
   * Navigate to all enum usages for the given enum type and field.
   *
   * @param enumType  The enum type to find.
   * @param enumField The enum field to find.
   * @param source    The source tree to search within.
   * @return An iterator over used/accessed enum field identities.
   * */
  private def goToEnumFieldUsage(enumType: Ast.TypeId,
                                 enumField: Ast.EnumField,
                                 source: Tree.Source): Iterator[Ast.Ident] =
    source
      .rootNode
      .walkDown
      .collect {
        // find all the selections matching the enum and the enum's field type.
        case Node(selector: Ast.EnumFieldSelector[_], _) if selector.enumId == enumType && selector.field == enumField.ident =>
          selector.field
      }

  /**
   * Navigate to all event field usages for an event at the index of the event field.
   *
   * @param eventDefId      The event definition ID to find.
   * @param eventFieldIndex The index of the event field.
   * @param source          The source tree to search within.
   * @return An iterator over expressions defined in position of the event field.
   */
  private def goToEventFieldUsage(eventDefId: Ast.TypeId,
                                  eventFieldIndex: Int,
                                  source: Tree.Source): Iterator[Ast.Expr[_]] =
    source
      .rootNode
      .walkDown
      .collect {
        // find all the event fields usages at given eventFieldIndex.
        case Node(emitEvent: Ast.EmitEvent[_], _) if emitEvent.id == eventDefId && eventFieldIndex < emitEvent.args.length =>
          emitEvent.args(eventFieldIndex)
      }

  /**
   * Navigate to all variable usages for the given variable identifier.
   *
   * @param ident The variable identifier to search for.
   * @param from  The node to search within, walking downwards.
   * @return An array sequence of variable usage IDs.
   */
  private def goToVariableUsages(ident: Ast.Ident,
                                 from: Node[Ast.Positioned]): ArraySeq[Ast.Ident] =
    from
      .walkDown
      .collect {
        // find all the selections matching the variable name.
        case Node(variable: Ast.Variable[_], _) if variable.id == ident =>
          variable.id
      }
      .to(ArraySeq)

  /**
   * Navigate to the nearest code block for which the given child node is in scope.
   *
   * @param childNode The node within the scoped block.
   * @return An Option containing the nearest parent block, if found.
   */
  private def goToNearestBlockInScope(childNode: Node[Ast.Positioned],
                                      source: Tree.Source): Option[Node[Ast.Positioned]] =
    source.ast match {
      case Left(_: Ast.Contract | _: Ast.ContractInterface | _: Ast.TxScript) =>
        // Find the nearest function definition or use the template body as the scope.
        goToNearestFuncDef(childNode)
          .map(_._1)
          .orElse(Some(source.rootNode))

      case Right(_: Ast.Struct) =>
        None
    }

  /**
   * Navigate to the nearest function definition for which the given child node is in scope.
   *
   * @param childNode The node to traverse up from.
   * @return An Option containing the nearest function definition, if found.
   */
  private def goToNearestFuncDef(childNode: Node[Ast.Positioned]): Option[(Node[Ast.Positioned], Ast.FuncDef[_])] =
    childNode
      .data
      .sourceIndex
      .flatMap {
        childNodeIndex =>
          childNode
            .walkParents
            .collectFirst {
              case node @ Node(function: Ast.FuncDef[_], _) if function.sourceIndex.exists(_ contains childNodeIndex.index) =>
                (node, function)
            }
      }

  /**
   * Navigate to the argument(s) of the nearest function to this node.
   *
   * @param childNode The node to traverse up in search of the function.
   * @param ident     The variable identifier to find arguments for.
   * @return An array sequence of [[Ast.Argument]]s matching the search result.
   * */
  private def goToNearestFunctionArguments(childNode: Node[Ast.Positioned],
                                           ident: Ast.Ident): Iterator[Ast.Argument] =
    goToNearestFuncDef(childNode)
      .iterator
      .flatMap {
        case (_, funcDef) =>
          funcDef
            .args
            .filter(_.ident == ident)
      }

  /**
   * Navigate to the template argument(s) for the given identifier.
   *
   * @param source The source tree to search within.
   * @param ident  The variable identifier to find arguments for.
   * @return An array sequence of [[Ast.Argument]]s matching the search result.
   * */
  private def goToTemplateArguments(source: Tree.Source,
                                    ident: Ast.Ident): Seq[Ast.Argument] = {
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
