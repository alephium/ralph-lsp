package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.Ast
import org.alephium.ralph.Ast.Positioned
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.ast.node.Node
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.pc.search.scope.ScopeBuilder

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
  def goTo(identNode: Node[Positioned],
           ident: Ast.Ident,
           source: Tree.Source): ArraySeq[Ast.Positioned] =
    identNode
      .parent // take one step up to check the type of ident node.
      .to(ArraySeq)
      .collect {
        case Node(variable: Ast.Variable[_], _) if variable.id == ident => // Is it a variable?
          // The user clicked on a variable. Take 'em there!
          goToVariable(
            variableNode = identNode,
            variable = variable,
            source = source
          )

        case Node(fieldSelector: Ast.EnumFieldSelector[_], _) if fieldSelector.field == ident =>
          // The user clicked on an enum field. Take 'em there!
          goToEnumField(
            fieldSelector = fieldSelector,
            source = source
          )

        case node @ Node(field: Ast.EnumField, _) if field.ident == ident =>
          // The user clicked on an enum field.
          // Check the parent to find the enum type.
          node
            .parent
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
      }
      .flatten

  /**
   * Navigate to the argument(s) for the given variable.
   *
   * @param variableNode The node representing the variable.
   * @param variable     The variable to find the argument for.
   * @param source       The source tree to search within.
   * @return An array sequence of [[Ast.Argument]]s matching the search result.
   * */
  private def goToVariable(variableNode: Node[Positioned],
                           variable: Ast.Variable[_],
                           source: Tree.Source): ArraySeq[Ast.Argument] =
    variable
      .sourceIndex
      .to(ArraySeq)
      .flatMap {
        variableIndex =>
          ScopeBuilder
            .buildArguments(source)
            .filter {
              argument =>
                argument.typeDef.ident == variable.id &&
                  argument.scope.contains(variableIndex.index)
            }
            .map(_.typeDef)
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
      case contract: Ast.Contract =>
        contract
          .enums
          .filter(_.id == fieldSelector.enumId)
          .flatMap(_.fields.find(_.ident == fieldSelector.field))
          .to(ArraySeq)

      case _: Ast.ContractInterface | _: Ast.TxScript =>
        ArraySeq.empty
    }

  /**
   * Navigate to all enum calls for the given enum type and field.
   *
   * @param enumType  The enum type to find.
   * @param enumField The enum field to find.
   * @param source    The source tree to search within.
   * @return An array sequence of enum field identities matching the search result.
   * */
  private def goToEnumFieldCalls(enumType: Ast.TypeId,
                                 enumField: Ast.EnumField,
                                 source: Tree.Source): ArraySeq[Ast.Ident] =
    source
      .rootNode
      .walkDown
      .collect {
        // find all the selections matching the enum and the enum's field type.
        case Node(selector: Ast.EnumFieldSelector[_], _) if selector.enumId == enumType && selector.field == enumField.ident =>
          selector.field
      }
      .to(ArraySeq)
}
