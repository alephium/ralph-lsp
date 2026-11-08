// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.cache.{SearchCache, WorkspaceSearchCache}
import org.alephium.ralph.lsp.pc.search.gotoref.GoToRefSetting
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeSearcher, SourceLocation}
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.ClientLogger

/** Additional semantic definitions used only while finding references. */
private object GoToDefReference {

  def search(
      identifier: Node[SoftAST.Identifier, SoftAST],
      source: SourceLocation.CodeSoft,
      cache: WorkspaceSearchCache,
      settings: GoToRefSetting,
      definitions: Vector[SourceLocation.NodeSoft[SoftAST.CodeString]]
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] = {
    val overrides =
      if (settings.includeTemplateArgumentOverrides)
        definitions.iterator.filter(_.ast eq identifier.data.code).flatMap(templateOverrides(_, cache))
      else
        Iterator.empty
    val fields =
      if (settings.includeEventFieldReferences)
        eventFields(identifier, source, cache, settings)
      else
        Iterator.empty
    overrides ++ fields
  }

  private def parameterName(expression: SoftAST.ExpressionAST): Option[SoftAST.CodeString] =
    expression match {
      case id: SoftAST.Identifier => Some(id.code)
      case binding: SoftAST.MutableBinding =>
        binding.identifier match {
          case id: SoftAST.Identifier => Some(id.code)
          case _                      => None
        }
      case assignment: SoftAST.TypeAssignment => parameterName(assignment.expressionLeft)
      case _                                  => None
    }

  private def templateOverrides(
      definition: SourceLocation.NodeSoft[SoftAST.CodeString],
      cache: WorkspaceSearchCache): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    definition.source.part match {
      case template: SoftAST.Template if template.params.exists(_.expressions.flatMap(parameterName).exists(_ == definition.ast)) =>
        val parents = SourceCodeSearcher.collectInheritedParents(definition.source, cache.trees)
        val children = cache.trees.filter {
          child =>
            SourceCodeSearcher.collectInheritedParents(child, cache.trees).contains(definition.source)
        }
        (parents.iterator ++ children.iterator).flatMap {
          parent =>
            parent.part match {
              case template: SoftAST.Template =>
                template
                  .params
                  .iterator
                  .flatMap(_.expressions)
                  .flatMap(parameterName)
                  .filter(_.text == definition.ast.text)
                  .map(SourceLocation.NodeSoft(_, parent))
              case _ => Iterator.empty
            }
        }
      case _ => Iterator.empty
    }

  private def eventFields(
      identifier: Node[SoftAST.Identifier, SoftAST],
      source: SourceLocation.CodeSoft,
      cache: WorkspaceSearchCache,
      settings: GoToRefSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    identifier
      .walkParents
      .collectFirst {
        case node @ Node(call: SoftAST.ReferenceCall, _) if node.parent.exists(_.data.isInstanceOf[SoftAST.Emit]) =>
          val argumentIndex = call.arguments.expressions.toSeq.indexWhere(_ == identifier.data)
          if (argumentIndex < 0)
            Iterator.empty
          else
            call.reference match {
              case id: SoftAST.Identifier =>
                GoToDefIdentifier(id.toNode.upcast(id), source, cache.workspace, settings.goToDefSetting.copy(goToRefSetting = None)).flatMap {
                  definition =>
                    definition
                      .ast
                      .toNode
                      .walkParents
                      .collectFirst {
                        case Node(event: SoftAST.Event, _) => event
                      }
                      .iterator
                      .flatMap(_.params.expressions.drop(argumentIndex).take(1))
                      .flatMap(parameterName)
                      .map(SourceLocation.NodeSoft(_, definition.source))
                }
              case _ => Iterator.empty
            }
      }
      .iterator
      .flatten

}
