// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.{Dependency, DependencyID}
import org.alephium.ralph.lsp.utils.{Node, URIUtil}
import org.alephium.ralph.lsp.utils.log.ClientLogger

/** Annotation names and property keys only resolve within native annotation metadata. */
private object GoToDefAnnotation {

  def apply(
      annotation: SoftAST.Annotation,
      field: Option[SoftAST.Identifier],
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    annotation.identifier match {
      case name: SoftAST.Identifier =>
        WorkspaceSearcher
          .collectAllDependencyTreesSoft(DependencyID.BuiltIn, workspace.build)
          .iterator
          .flatMap(_._2)
          .filter(
            source => URIUtil.getFileName(source.parsed.fileURI) == Dependency.ANNOTATION_FILE_NAME
          )
          .flatMap {
            source =>
              source
                .part
                .toNode
                .walk
                .collect {
                  case Node(function: SoftAST.Function, _) => function.signature
                }
                .flatMap {
                  signature =>
                    signature.fnName match {
                      case id: SoftAST.Identifier if id.code.text == name.code.text =>
                        val matches = field match {
                          case None => Iterator.single(id)
                          case Some(key) =>
                            signature
                              .params
                              .toNode
                              .walk
                              .collect {
                                case Node(assignment: SoftAST.TypeAssignment, _) => assignment.expressionLeft
                              }
                              .collect {
                                case id: SoftAST.Identifier if id.code.text == key.code.text => id
                              }
                        }
                        matches.map(
                          id => SourceLocation.NodeSoft(id.code, source)
                        )
                      case _ => Iterator.empty
                    }
                }
          }
      case _ => Iterator.empty
    }

}
