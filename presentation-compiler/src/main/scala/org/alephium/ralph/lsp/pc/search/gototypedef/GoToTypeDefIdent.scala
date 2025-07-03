// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gototypedef

import org.alephium.ralph.{Ast, Type}
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.Node

import scala.collection.immutable.ArraySeq

case object GoToTypeDefIdent extends StrictImplicitLogging {

  /**
   * Searches type-definitions given the identifier node [[Ast.Ident]].
   *
   * @param node      The node representing the identifier being searched.
   * @param workspace The workspace state where the source-code is located.
   * @return An iterator over type-definition search results.
   */
  def goToIdent(
      node: Node[Ast.Ident, Ast.Positioned],
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): ArraySeq[SourceLocation.GoToTypeDef] =
    node.parent match {
      case Some(Node(variable: Ast.Variable[_], _)) =>
        searchCachedType(
          cachedType = variable.getCachedType(),
          workspace = workspace
        )

      case Some(node @ Node(namedVar: Ast.NamedVar, _)) =>
        goToNamedVar(
          node = node.upcast(namedVar),
          namedVar = namedVar,
          workspace = workspace
        )

      case Some(Node(data, _)) =>
        logger.info(s"${this.productPrefix} not implemented for ${data.getClass.getName}. SourceIndex: ${data.sourceIndex}")
        ArraySeq.empty

      case None =>
        logger.info(s"${this.productPrefix}: Type information not found for node: ${node.data.getClass.getName}. SourceIndex: ${node.data.sourceIndex}")
        ArraySeq.empty
    }

  /**
   * Searches type-definitions given the named variable node [[Ast.NamedVar]].
   *
   * @param node      The node representing the named variable being searched.
   * @param workspace The workspace state where the source-code is located.
   * @return An iterator over type-definition search results.
   */
  def goToNamedVar(
      node: Node[Ast.NamedVar, Ast.Positioned],
      namedVar: Ast.NamedVar,
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): ArraySeq[SourceLocation.GoToTypeDef] =
    node.parent match {
      case Some(Node(varDef: Ast.VarDef[_], _)) =>
        searchTupledVarDef(
          target = namedVar,
          varDef = varDef,
          workspace = workspace
        )

      case Some(Node(data, _)) =>
        logger.info(s"${this.productPrefix} not implemented for ${data.getClass.getName}. SourceIndex: ${data.sourceIndex}")
        ArraySeq.empty

      case None =>
        logger.info(s"${this.productPrefix}: Type information not found for node: ${node.data.getClass.getName}. SourceIndex: ${node.data.sourceIndex}")
        ArraySeq.empty
    }

  /**
   * Searches type-definitions given the anonymous variable node [[Ast.AnonymousVar]].
   *
   * @param anonVarNode The node representing the anonymous variable being searched.
   * @param workspace   The workspace state where the source-code is located.
   * @return An iterator over type-definition search results.
   */
  def goToAnonymousVar(
      anonVarNode: Node[Ast.AnonymousVar, Ast.Positioned],
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): ArraySeq[SourceLocation.GoToTypeDef] =
    anonVarNode.parent match {
      case Some(Node(varDef: Ast.VarDef[_], _)) =>
        searchTupledVarDef(
          target = anonVarNode.data,
          varDef = varDef,
          workspace = workspace
        )

      case Some(Node(data, _)) =>
        logger.info(s"${this.productPrefix} not implemented for ${data.getClass.getName}. SourceIndex: ${data.sourceIndex}")
        ArraySeq.empty

      case None =>
        logger.info(s"${this.productPrefix}: Type information not found for node: ${anonVarNode.data.getClass.getName}. SourceIndex: ${anonVarNode.data.sourceIndex}")
        ArraySeq.empty
    }

  /**
   * Handles tupled variable declarations.
   *
   * Example:
   * {{{
   *   let (a, b, c) = foo()
   * }}}
   *
   * @param target    The identifier for one of the tuple elements being searched.
   * @param varDef    The variable definition representing the full tuple declaration.
   * @param workspace The workspace state containing the source code.
   * @return Searched type-definition result for the specified element.
   */
  private def searchTupledVarDef(
      target: Ast.VarDeclaration,
      varDef: Ast.VarDef[_],
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): ArraySeq[SourceLocation.GoToTypeDef] = {
    // Find index of
    val indexOfTarget =
      varDef.vars.indexWhere {
        varDec =>
          // SourceIndex check is required, especially for `Ast.AnonymousVar`
          // because multiple instances of it will always be considered equal.
          // For example, in `let (_, _, _) = foo()`, all three placeholders
          // correspond to the same anonymous variable instance.
          varDec == target && varDec.sourceIndex.exists(target.sourceIndex.contains)
      }

    val typeOfTargetNode =
      varDef
        .value
        .getCachedType()
        .flatMap {
          types =>
            // See issue #1292 on dev alephium - It's safe to assume a maximum of one element in types.
            types.headOption flatMap {
              case Type.Struct(id) =>
                workspace match {
                  // Structs are stored in ralphc's global state, which is created when the ralphc compiler is executed.
                  // Therefore, the workspace state must be IsCompiled.
                  case compiled: WorkspaceState.IsCompiled =>
                    compiled.compilerRunGlobalState flatMap {
                      state =>
                        state
                          .getStruct(id)
                          .flatMap(_.fields.lift(indexOfTarget))
                          .map(_.tpe)
                    }

                  case _ =>
                    None
                }

              case _ =>
                // Fetch the `Type` at the position-index of the target.
                types.lift(indexOfTarget)
            }
        }

    searchCachedType(
      cachedType = typeOfTargetNode.map(Seq(_)),
      workspace = workspace
    )
  }

  private def searchCachedType(
      cachedType: Option[Seq[Type]],
      workspace: WorkspaceState.IsSourceAware
    )(implicit logger: ClientLogger): ArraySeq[SourceLocation.GoToTypeDef] =
    cachedType match {
      case Some(types) =>
        searchTypes(
          types = types,
          workspace = workspace
        )

      case None =>
        logger.trace("Type information not found in node's AST")
        ArraySeq.empty
    }

  private def searchTypes(
      types: Seq[Type],
      workspace: WorkspaceState.IsSourceAware): ArraySeq[SourceLocation.GoToTypeDef] =
    WorkspaceSearcher
      .collectTypes(
        types = types,
        workspace = workspace
      )
      .map {
        case (typeId, code) =>
          SourceLocation.GoToTypeDef(
            ast = typeId,
            source = code
          )
      }

}
