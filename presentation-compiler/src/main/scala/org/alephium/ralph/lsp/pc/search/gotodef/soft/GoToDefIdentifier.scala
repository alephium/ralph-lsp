// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef.soft

import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.gotodef.{GoToDefSetting, ScopeWalker}
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeSearcher, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.cache.{SearchCache, WorkspaceSearchCache}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

private object GoToDefIdentifier extends StrictImplicitLogging {

  /**
   * Searches definitions given the location of the identifier node [[SoftAST.Identifier]]
   * and the [[SourceLocation]] of the identifier.
   *
   * @param identNode  The node representing the identifier being searched.
   * @param sourceCode The block-part and its source code state where this search is executed.
   * @param workspace  The workspace state where the source-code is located.
   * @return An iterator over definition search results.
   */
  def apply(
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      workspace: WorkspaceState.IsSourceAware,
      settings: GoToDefSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.GoToDefSoft] =
    searchParent(
      identNode = identNode,
      parent = identNode.parent,
      sourceCode = sourceCode,
      cache = searchCache.get(workspace),
      settings = settings
    )

  /**
   * Searches definitions given the location of the identifier node [[SoftAST.Identifier]]
   * and any of its parent or grandparent nodes in the hierarchy.
   *
   * Steps:
   *  - First, checks if the current [[SoftAST.Identifier]] itself belongs to a definition.
   *    These are self-jump-definitions.
   *  - If the above condition is not met, then it executes full search within the workspace.
   *
   * @param identNode  The node representing the identifier being searched.
   * @param parent     One of the identifier node's parents.
   * @param sourceCode The block-part and its source code state where this search is executed.
   * @param cache      The workspace state and its cached trees.
   * @return An iterator over definition search results.
   */
  private def searchParent(
      identNode: Node[SoftAST.Identifier, SoftAST],
      parent: Option[Node[SoftAST, SoftAST]],
      sourceCode: SourceLocation.CodeSoft,
      cache: WorkspaceSearchCache,
      settings: GoToDefSetting
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.GoToDefSoft] = {
    @inline def runFullSearch() =
      search(
        identNode = identNode,
        sourceCode = sourceCode,
        cache = cache,
        settings = settings,
        detectCallSyntax = true
      )

    @inline def self() =
      Iterator.single(
        SourceLocation.NodeSoft(
          ast = identNode.data.code,
          source = sourceCode
        )
      )

    parent match {
      case Some(Node(assignment: SoftAST.StructFieldAssignment, _)) if assignment.expressionLeft contains identNode.data =>
        // TODO: Process struct field assignments - Issue https://github.com/alephium/ralph-lsp/issues/322
        Iterator.empty

      case Some(Node(annotation: SoftAST.Annotation, _)) if annotation.identifier contains identNode.data =>
        // TODO: Support jump-definition for annotations: https://github.com/alephium/ralph-lsp/issues/589
        Iterator.empty

      case Some(Node(assignment: SoftAST.TypeAssignment, _)) if assignment.expressionLeft == identNode.data =>
        self()

      case Some(Node(assignment: SoftAST.MutableBinding, _)) if assignment.identifier == identNode.data =>
        self()

      // Full search must be executed when `includeAbstractFuncDef == true`
      case Some(Node(function: SoftAST.FunctionSignature, _)) if !settings.includeAbstractFuncDef && function.fnName == identNode.data =>
        self()

      case Some(Node(template: SoftAST.Template, _)) if template.identifier == identNode.data =>
        self()

      case Some(Node(enumAST: SoftAST.Enum, _)) if enumAST.identifier == identNode.data =>
        self()

      case Some(Node(event: SoftAST.Event, _)) if event.identifier == identNode.data =>
        self()

      case Some(Node(struct: SoftAST.Struct, _)) if struct.identifier == identNode.data =>
        self()

      case Some(Node(map: SoftAST.MapAssignment, _)) if map.identifier == identNode.data =>
        self()

      case Some(node @ Node(assignment: SoftAST.Assignment, _)) if assignment.expressionLeft == identNode.data =>
        node.parent match {
          // If it's an assignment, it must also be a variable declaration for the current node to be a self.
          case Some(Node(_: SoftAST.VariableDeclaration | _: SoftAST.Const, _)) =>
            self()

          case Some(grandParentNode @ Node(_: SoftAST.Group[_, _, _], _)) =>
            grandParentNode.parent match {
              case Some(Node(_: SoftAST.Annotation, _)) =>
                // TODO: Support jump-definition for annotations: https://github.com/alephium/ralph-lsp/issues/589
                Iterator.empty

              case _ =>
                // invoke full scope search.
                runFullSearch()
            }

          case _ =>
            // invoke full scope search.
            runFullSearch()
        }

      case Some(node @ Node(group: SoftAST.Group[_, _, _], _)) =>
        searchGroup(
          group = node.upcast(group),
          identNode = identNode,
          sourceCode = sourceCode,
          cache = cache,
          settings = settings,
          detectCallSyntax = true
        )

      case Some(node @ Node(tail: SoftAST.GroupTail[_], _)) =>
        node.parent match {
          case Some(node @ Node(group: SoftAST.Group[_, _, _], _)) => // a `GroupTail` is always contained with a `Group`
            searchGroup(
              group = node.upcast(group),
              identNode = identNode,
              sourceCode = sourceCode,
              cache = cache,
              settings = settings,
              detectCallSyntax = true
            )

          case _ =>
            logger.trace(s"GroupTail not contained with a group. Index: ${tail.index}. File: ${sourceCode.parsed.fileURI}")
            runFullSearch()
        }

      case Some(node @ Node(_: SoftAST.ReferenceCall, _)) =>
        node.parent match {
          case Some(node @ Node(methodCall: SoftAST.MethodCall, _)) =>
            searchMethodCall(
              methodCallNode = node.upcast(methodCall),
              identNode = identNode,
              sourceCode = sourceCode,
              cache = cache,
              settings = settings,
              detectCallSyntax = true
            )

          case _ =>
            runFullSearch()
        }

      case Some(node @ Node(methodCall: SoftAST.MethodCall, _)) =>
        searchMethodCall(
          methodCallNode = node.upcast(methodCall),
          identNode = identNode,
          sourceCode = sourceCode,
          cache = cache,
          settings = settings,
          detectCallSyntax = true
        )

      case _ =>
        runFullSearch()
    }
  }

  /**
   * Searches for occurrences of the given identifier node within the source code.
   *
   * @param identNode        The identifier node to search for.
   * @param sourceCode       The source code state where the search is performed.
   * @param detectCallSyntax If `true`, ensures that when a function is called,
   *                         the search is restricted to reference calls only and does not
   *                         return variables with the same name.
   * @return An iterator over the locations of the definitions.
   */
  @tailrec
  private def search(
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      cache: WorkspaceSearchCache,
      settings: GoToDefSetting,
      detectCallSyntax: Boolean
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] = {
    val inherited =
      if (settings.includeInheritance)
        searchInheritance(
          target = identNode,
          sourceCode = sourceCode,
          cache = cache,
          includeAbstractFuncDef = settings.includeAbstractFuncDef,
          detectCallSyntax = detectCallSyntax
        )
      else
        Iterator.empty

    val local =
      searchLocal(
        from = sourceCode.part.toNode,
        target = identNode,
        sourceCode = sourceCode,
        detectCallSyntax = detectCallSyntax,
        enableAssignmentSearch = false
      )

    val globals =
      searchGlobal(
        target = identNode,
        trees = cache.trees.iterator,
        detectCallSyntax = detectCallSyntax
      )

    val result = (local.iterator ++ inherited ++ globals).distinct

    if (detectCallSyntax && result.isEmpty)
      search(
        identNode = identNode,
        sourceCode = sourceCode,
        cache = cache,
        settings = settings,
        detectCallSyntax = false // The restricted exact call syntax search returned no results. Executing a relaxed search.
      )
    else
      result
  }

  /**
   * Searches for occurrences of the given identifier node within global scope.
   *
   * @param target The identifier node to search for.
   * @param trees  All workspace code within scope.
   * @return An iterator over the locations of the definitions.
   */
  private def searchGlobal(
      target: Node[SoftAST.Identifier, SoftAST],
      trees: Iterator[SourceLocation.CodeSoft],
      detectCallSyntax: Boolean): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    trees flatMap {
      sourceCode =>
        searchGlobal(
          target = target,
          tree = sourceCode.part,
          sourceCode = sourceCode,
          detectCallSyntax = detectCallSyntax
        )
    }

  /**
   * Searches for occurrences of the given identifier node within global scope.
   *
   * @param target The identifier node to search for.
   * @param tree   A tree within the global scope.
   * @return An iterator over the locations of the definitions.
   */
  private def searchGlobal(
      target: Node[SoftAST.Identifier, SoftAST],
      tree: SoftAST.BlockPartAST,
      sourceCode: SourceLocation.CodeSoft,
      detectCallSyntax: Boolean): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    tree match {
      case template: SoftAST.Template =>
        searchTemplateIdentifier(
          templateIdentifier = template.identifier,
          target = target,
          sourceCode = sourceCode,
          detectCallSyntax = detectCallSyntax
        )

      case struct: SoftAST.Struct =>
        searchStructIdentifier(
          structIdentifier = struct.identifier,
          target = target,
          sourceCode = sourceCode,
          detectCallSyntax = detectCallSyntax
        )

      case event: SoftAST.Event =>
        searchEvent(
          event = event,
          target = target,
          sourceCode = sourceCode,
          detectCallSyntax = detectCallSyntax
        )

      case enumAST: SoftAST.Enum =>
        searchEnum(
          enumAST = enumAST,
          target = target,
          sourceCode = sourceCode,
          detectCallSyntax = detectCallSyntax
        )

      case constant: SoftAST.Const =>
        searchConstant(
          constant = constant,
          target = target,
          sourceCode = sourceCode,
          detectCallSyntax = detectCallSyntax
        )

      case _ =>
        Iterator.empty
    }

  /**
   * Expands and searches for all possible local definitions starting from the given position.
   *
   * @param from                   The position from where the search should begin.
   * @param target                 The identifier being searched.
   * @param sourceCode             The source code state where the `from` node is located.
   * @param detectCallSyntax       If `true`, ensures that when a function is called,
   *                               the search is restricted to reference calls only and does not
   *                               return variables with the same name.
   * @param enableAssignmentSearch If `true`, search all left-hand side expressions in [[SoftAST.Assignment]].
   * @return An iterator over the locations of the definitions.
   */
  private def searchLocal(
      from: Node[SoftAST, SoftAST],
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      detectCallSyntax: Boolean,
      enableAssignmentSearch: Boolean): Iterable[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    // Reference calls are then ones ending with parentheses, for example `refCall()`.
    // Reference calls should only search for function and contract calls, not variables.
    ScopeWalker.walk(
      from = from,
      anchor = target.data.index
    ) {
      case Node(variable: SoftAST.VariableDeclaration, _) if !detectCallSyntax || (!target.is_RefCall_StructCont_Or_TypeAssignsType() && !target.isWithinEmit()) =>
        searchExpression(
          expression = variable,
          target = target,
          sourceCode = sourceCode
        )

      case Node(assignment: SoftAST.TypeAssignment, _) if !detectCallSyntax || (!target.is_RefCall_StructCont_Or_TypeAssignsType() && !target.isWithinEmit()) =>
        searchExpression(
          expression = assignment,
          target = target,
          sourceCode = sourceCode
        )

      case Node(map: SoftAST.MapAssignment, _) if !detectCallSyntax || (!target.is_RefCall_StructCont_Or_TypeAssignsType() && !target.isWithinEmit()) =>
        searchExpression(
          expression = map,
          target = target,
          sourceCode = sourceCode
        )

      case Node(binding: SoftAST.MutableBinding, _) if !detectCallSyntax || (!target.is_RefCall_StructCont_Or_TypeAssignsType() && !target.isWithinEmit()) =>
        searchExpression(
          expression = binding,
          target = target,
          sourceCode = sourceCode
        )

      case Node(assignment: SoftAST.Assignment, _) if enableAssignmentSearch && (!detectCallSyntax || !target.is_RefCall_StructCont_Or_TypeAssignsType()) =>
        // Used for enums. Only enums contains immutable assignments, which are basically variable definitions.
        searchExpression(
          expression = assignment,
          target = target,
          sourceCode = sourceCode
        )

      case Node(function: SoftAST.Function, _) =>
        searchFunction(
          function = function,
          target = target,
          sourceCode = sourceCode,
          detectCallSyntax = detectCallSyntax
        )

      case Node(template: SoftAST.Template, _) =>
        searchTemplate(
          template = template,
          target = target,
          sourceCode = sourceCode,
          detectCallSyntax = detectCallSyntax
        )

      case Node(event: SoftAST.Event, _) =>
        searchEvent(
          event = event,
          target = target,
          sourceCode = sourceCode,
          detectCallSyntax = detectCallSyntax
        )

      case Node(constant: SoftAST.Const, _) =>
        searchConstant(
          constant = constant,
          target = target,
          sourceCode = sourceCode,
          detectCallSyntax = detectCallSyntax
        )

      case Node(enumAST: SoftAST.Enum, _) =>
        searchEnum(
          enumAST = enumAST,
          target = target,
          sourceCode = sourceCode,
          detectCallSyntax = detectCallSyntax
        )

      case Node(event: SoftAST.Struct, _) =>
        searchStructIdentifier(
          structIdentifier = event.identifier,
          target = target,
          sourceCode = sourceCode,
          detectCallSyntax = detectCallSyntax
        )
    }

  /**
   * Given a target identifier, searches all inherited contracts for all possible definitions, including
   * the built-in interfaces.
   *
   * @param target           The identifier being searched.
   * @param sourceCode       The source code state where the `target` node is located.
   * @param cache            The workspace state and its cached trees.
   * @param detectCallSyntax If `true`, ensures that when a function is called,
   *                         the search is restricted to reference calls only and does not
   *                         return variables with the same name.
   * @return An iterator over the locations of the definitions.
   */
  private def searchInheritance(
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      cache: WorkspaceSearchCache,
      includeAbstractFuncDef: Boolean,
      detectCallSyntax: Boolean
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] = {
    // The actual inherited code as defined in the AST
    val parents =
      SourceCodeSearcher.collectInheritedParents(
        source = sourceCode,
        allSource = cache.trees
      )

    // Also collect implementing children when `includeAbstractFuncDef == true`
    val children =
      if (includeAbstractFuncDef)
        SourceCodeSearcher.collectImplementingChildren(
          source = sourceCode,
          allSource = cache.trees
        )
      else
        ArraySeq.empty

    val inheritedTrees =
      parents ++ children

    // Inheritance search must include all built-in interfaces.
    val builtInTrees =
      sourceCode.part match {
        case _: SoftAST.Enum | _: SoftAST.Event | _: SoftAST.Struct =>
          // Disable for enum, structs, events
          ArraySeq.empty

        case _ =>
          // Allow built-in searches for everything else
          cache.builtInTrees
      }

    // Merge both the actual inherited trees and the built-in trees.
    val allInheritedTrees =
      inheritedTrees.iterator ++ builtInTrees

    // Execute inheritance search.
    searchInheritance(
      target = target,
      inheritance = allInheritedTrees,
      detectCallSyntax = detectCallSyntax
    )
  }

  /**
   * Given a target identifier, searches all inherited contracts for all possible definitions.
   *
   * @param target           The identifier being searched.
   * @param inheritance      The inherited source code to search within.
   * @param detectCallSyntax If `true`, ensures that when a function is called,
   *                         the search is restricted to reference calls only and does not
   *                         return variables with the same name.
   * @return An iterator over the locations of the definitions.
   */
  private def searchInheritance(
      target: Node[SoftAST.Identifier, SoftAST],
      inheritance: Iterator[SourceLocation.CodeSoft],
      detectCallSyntax: Boolean
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    inheritance flatMap {
      sourceCode =>
        searchInheritance(
          target = target,
          inherited = sourceCode,
          detectCallSyntax = detectCallSyntax
        )
    }

  /**
   * Given a target identifier, searches an inherited contract for all possible definitions.
   *
   * Note that the `target` identifier belongs to the source code where this search is executed.
   * Since the inherited source code does not recognise this target,
   * this function creates a virtual identifier at the end of the inherited source code.
   * This allows a local search that returns all public definitions.
   *
   * {{{
   *   Contract Child() extends Parent() {
   *      let copy = pa@@ram // `param` is not visible to `Parent`
   *   }
   *
   *   Abstract Contract Parent(>>param<<: Type) {
   *     pa@@ram // A virtual identifier is created injected into this source's tree, and a local search is executed to find `>>param<<`.
   *   }
   * }}}
   *
   * @param target           The identifier being searched.
   * @param inherited        The inherited source code to search within.
   * @param detectCallSyntax If `true`, ensures that when a function is called,
   *                         the search is restricted to reference calls only and does not
   *                         return variables with the same name.
   * @return All found definitions.
   */
  private def searchInheritance(
      target: Node[SoftAST.Identifier, SoftAST],
      inherited: SourceLocation.CodeSoft,
      detectCallSyntax: Boolean
    )(implicit logger: ClientLogger): Iterable[SourceLocation.NodeSoft[SoftAST.CodeString]] = {
    val virtualNode =
      buildVirtualNodeAtSourceEnd(
        target = target,
        source = inherited
      )

    virtualNode match {
      case Some(virtualNode) =>
        // execute a local search within the inherited source code
        searchLocal(
          from = inherited.part.toNode,
          target = virtualNode,
          sourceCode = inherited,
          detectCallSyntax = detectCallSyntax,
          enableAssignmentSearch = false
        )

      case None =>
        // This should not occur if the above AST has an identifier.
        logger.error(s"Error: Virtual `Identifier` not found. File: ${inherited.parsed.fileURI}")
        Iterable.empty
    }
  }

  /**
   * Builds a virtual AST node by duplicating the given [[SoftAST.Identifier]] tree and
   * updating its [[SourceIndex]] such that its placed the end of the source.
   *
   * @param target The identifier node to duplicate.
   * @param source The source location - the node will be placed at its end.
   * @return A new virtual node at the end of the source, or [[None]] if creating fails.
   */
  private def buildVirtualNodeAtSourceEnd(
      target: Node[SoftAST.Identifier, SoftAST],
      source: SourceLocation.CodeSoft): Option[Node[SoftAST.Identifier, SoftAST]] =
    buildVirtualNode(
      target = target,
      sourceIndex = point(source.part.index.to)
    )

  /**
   * Builds a virtual AST node by duplicating the given [[SoftAST.Identifier]] tree and
   * assigning it the given [[SourceIndex]]. A maximum of two parent nodes are duplicated.
   *
   * A virtual node is created only for the following syntax:
   *  - [[SoftAST.Identifier]] — e.g. `Transfer`
   *  - [[SoftAST.ReferenceCall]] — e.g. `Transfer()`
   *  - [[SoftAST.MethodCall]] — e.g. `Transfer.function`
   *  - [[SoftAST.Emit]] — e.g. `emit <<any of the above three cases>>`
   *
   * @note This function is used for searching within inheritance.
   *       For details see the documentation of the function [[searchInheritance]].
   *
   * @param target      The identifier node to duplicate.
   * @param sourceIndex The source position to assign to each node.
   * @return The created virtual expression.
   */
  private def buildVirtualNode(
      target: Node[SoftAST.Identifier, SoftAST],
      sourceIndex: SourceIndex): Option[Node[SoftAST.Identifier, SoftAST]] = {
    val emitUpdated =
      target
        .walkParents
        .take(2) // The identifier could be wrapped around an `emit` call, which can exist in the parent or the grandparent
        .collectFirst {
          case Node(emit: SoftAST.Emit, _) => // If the `emit` exists, deep-copy from that node.
            emit.deepCopy(sourceIndex)
        }

    val copiedTree =
      emitUpdated match {
        case Some(emit) =>
          emit

        case None => // `Emit` does not exist.
          target.parent match {
            case Some(Node(exp @ (_: SoftAST.ReferenceCall | _: SoftAST.MethodCall), _)) =>
              // MethodCall - Allow enums through `MyEnu@@m.One`. TODO: Need more user/behaviour testing.
              // If the identifier is wrapped around a reference call, deep-copy the reference call.
              exp.deepCopy(sourceIndex)

            case _ =>
              // Otherwise, deep copy the identifier.
              target.data.deepCopy(sourceIndex)
          }
      }

    // Return the target identifier contained in the new tree.
    copiedTree
      .toNode
      .walkDown
      .collectFirst {
        case node @ Node(ident: SoftAST.Identifier, _) if ident.code.text == target.data.code.text =>
          node.upcast(ident)
      }
  }

  /**
   * Given a function, expands and searches within it for all possible definitions.
   *
   * @param function         The function to expand and search.
   * @param target           The identifier being searched.
   * @param sourceCode       The source code state where the function belongs.
   * @param detectCallSyntax If `true`, ensures that when a function is called,
   *                         the search is restricted to reference calls only and does not
   *                         return variables with the same name.
   * @return An iterator over the locations of the definitions.
   */
  private def searchFunction(
      function: SoftAST.Function,
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      detectCallSyntax: Boolean): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] = {
    // If the identifier belongs the function's block, search the parameters and the block.
    val blockMatches =
      function.block match {
        case Some(block) if block.contains(target) =>
          // Search the parameters
          val paramMatches =
            if (detectCallSyntax && (target.is_RefCall_StructCont_Or_TypeAssignsType() || target.isWithinEmit()))
              Iterator.empty
            else
              searchExpression(
                expression = function.signature.params,
                target = target,
                sourceCode = sourceCode
              )

          // search the block
          val blockMatches =
            searchBlock(
              block = block,
              target = target,
              sourceCode = sourceCode,
              detectCallSyntax = detectCallSyntax,
              enableAssignmentSearch = false
            )

          paramMatches ++ blockMatches

        case _ =>
          Iterator.empty
      }

    // Check if the name matches the identifier.
    val nameMatches =
      if (!detectCallSyntax || (target.isReferenceCall() && !target.isWithinEmit()))
        searchIdentifier(
          identifier = function.signature.fnName,
          target = target,
          sourceCode = sourceCode
        )
      else
        Iterator.empty

    nameMatches ++ blockMatches
  }

  /**
   * Given an enum, expands and searches within it for all possible definitions.
   *
   * @param enumAST          The enum to expand and search.
   * @param target           The identifier being searched.
   * @param sourceCode       The source code state where the enum belongs.
   * @param detectCallSyntax If `true`, ensures that when an enum is called,
   *                         the search is restricted to enum calls only.
   * @return An iterator over the locations of the definitions.
   */
  private def searchEnum(
      enumAST: SoftAST.Enum,
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      detectCallSyntax: Boolean): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] = {
    val blockMatches =
      enumAST.block match {
        case Some(block) if block.contains(target) =>
          // search the block
          searchBlock(
            block = block,
            target = target,
            sourceCode = sourceCode,
            detectCallSyntax = detectCallSyntax,
            enableAssignmentSearch = true
          )

        case _ =>
          Iterator.empty
      }

    // Check if the name matches the identifier.
    val nameMatches =
      if (!detectCallSyntax || (target.isMethodCall() && !target.isWithinEmit()))
        searchIdentifier(
          identifier = enumAST.identifier,
          target = target,
          sourceCode = sourceCode
        )
      else
        Iterator.empty

    nameMatches ++ blockMatches
  }

  /**
   * Given a template, expands and searches within it for all possible definitions.
   *
   * @param template         The template to expand and search.
   * @param target           The identifier being searched.
   * @param sourceCode       The source code state where the function belongs.
   * @param detectCallSyntax If `true`, restricts the search to syntax that matches the template being operated on.
   * @return An iterator over the locations of the definitions.
   */
  private def searchTemplate(
      template: SoftAST.Template,
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      detectCallSyntax: Boolean): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] = {
    // Search within the parameters if either of the following conditions is met:
    //  - The target belongs to the template's block
    //  - The target belongs to the template's inheritance group
    val paramMatches =
      if (template.block.exists(_.contains(target)) || template.inheritance.exists(_.contains(target)))
        if (detectCallSyntax && (target.is_RefCall_StructCont_Or_TypeAssignsType() || target.isWithinEmit()))
          Iterator.empty
        else
          template.params match {
            case Some(params) =>
              searchExpression(
                expression = params,
                target = target,
                sourceCode = sourceCode
              )

            case None =>
              Iterator.empty
          }
      else
        Iterator.empty

    // Search within the block only if it contains the target.
    val blockMatches =
      template.block match {
        case Some(block) if block.contains(target) =>
          // search the block
          searchBlock(
            block = block,
            target = target,
            sourceCode = sourceCode,
            detectCallSyntax = detectCallSyntax,
            enableAssignmentSearch = false
          )

        case _ =>
          Iterator.empty
      }

    // Check if the name matches the identifier.
    val nameMatches =
      searchTemplateIdentifier(
        templateIdentifier = template.identifier,
        target = target,
        sourceCode = sourceCode,
        detectCallSyntax = detectCallSyntax
      )

    nameMatches ++ blockMatches ++ paramMatches
  }

  /**
   * Given a template identifier, expands and searches it for a possible target identifier definition.
   *
   * @param templateIdentifier The template identifier to search.
   * @param target             The identifier being searched.
   * @param sourceCode         The source code state where the template identifier belongs.
   * @param detectCallSyntax   If `true`, restricts the search to syntax the matches the template being operated on.
   * @return An iterator over the locations of the definitions.
   */
  private def searchTemplateIdentifier(
      templateIdentifier: SoftAST.IdentifierAST,
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      detectCallSyntax: Boolean): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    // Check if the name matches the identifier.
    if (!detectCallSyntax || (target.is_RefCall_Or_TypeAssignsType() && !target.isWithinEmit()))
      searchIdentifier(
        identifier = templateIdentifier.identifier,
        target = target,
        sourceCode = sourceCode
      )
    else
      Iterator.empty

  /**
   * Given a struct identifier, expands and searches it for a possible target identifier definition.
   *
   * @param structIdentifier The struct identifier to search.
   * @param target           The identifier being searched.
   * @param sourceCode       The source code state where the struct identifier belongs.
   * @param detectCallSyntax If `true`, restricts the search to syntax the matches the struct being operated on.
   *                         For example, struct constructor or type assignment:
   *                         {{{
   *                           // Constructor
   *                           TheStruct@@ {
   *                             field1: ...,
   *                             field2: ...
   *                           }
   *
   *                           // Type assignment
   *                           fn function(param: TheStruct@@)
   *                         }}}
   *
   * @return An iterator over the locations of the definitions.
   */
  private def searchStructIdentifier(
      structIdentifier: SoftAST.IdentifierAST,
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      detectCallSyntax: Boolean): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    // Check if the name matches the identifier.
    if (!detectCallSyntax || (target.is_StructCont_Or_TypeAssignsType() && !target.isWithinEmit()))
      searchIdentifier(
        identifier = structIdentifier.identifier,
        target = target,
        sourceCode = sourceCode
      )
    else
      Iterator.empty

  /**
   * Given an event, expands and searches within it for all possible definitions.
   *
   * @param event      The event to expand and search.
   * @param target     The identifier being searched.
   * @param sourceCode The source code state where the event belongs.
   * @return An iterator over the locations of the definitions.
   */
  private def searchEvent(
      event: SoftAST.Event,
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      detectCallSyntax: Boolean): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    if (!detectCallSyntax || (target.isWithinEmit() && !target.isMethodCall()))
      searchIdentifier(
        identifier = event.identifier,
        target = target,
        sourceCode = sourceCode
      )
    else
      Iterator.empty

  /**
   * Given a constant, expands and searches within it for all possible definitions.
   *
   * @param constant   The constant to expand and search.
   * @param target     The identifier being searched.
   * @param sourceCode The source code state where the event belongs.
   * @return An iterator over the locations of the definitions.
   */
  private def searchConstant(
      constant: SoftAST.Const,
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      detectCallSyntax: Boolean): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    if (!detectCallSyntax || (!target.is_RefCall_StructCont_TypeAssignsType_Or_MethodCall() && !target.isWithinEmit()))
      searchExpression(
        expression = constant.assignment.expressionLeft,
        target = target,
        sourceCode = sourceCode
      )
    else
      Iterator.empty

  /**
   * Given a block, expands and searches within it for all possible definitions.
   *
   * @param block            The block to expand and search.
   * @param target           The identifier being searched.
   * @param sourceCode       The source code state where the function belongs.
   * @param detectCallSyntax If `true`, ensures that when a function is called,
   *                         the search is restricted to reference calls only and does not
   *                         return variables with the same name.
   * @return An iterator over the locations of the definitions.
   */
  private def searchBlock(
      block: SoftAST.Block,
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      detectCallSyntax: Boolean,
      enableAssignmentSearch: Boolean): Iterable[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    if (block contains target)
      searchLocal(
        from = block.toNode,
        target = target,
        sourceCode = sourceCode,
        detectCallSyntax = detectCallSyntax,
        enableAssignmentSearch = enableAssignmentSearch
      )
    else
      Iterable.empty

  /**
   * Expands and searches the given group.
   *
   * @param group            The node representing the group being searched.
   * @param identNode        The node representing the identifier being searched, which is also a child of the group.
   * @param sourceCode       The block-part and its source code state where this search is executed.
   * @param cache            The workspace state and its cached trees.
   * @param detectCallSyntax If `true`, ensures that when a function is called,
   *                         the search is restricted to reference calls only and does not
   *                         return variables with the same name.
   * @return An iterator over definition search results.
   */
  private def searchGroup(
      group: Node[SoftAST.Group[_, _, _], SoftAST],
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      cache: WorkspaceSearchCache,
      settings: GoToDefSetting,
      detectCallSyntax: Boolean
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    group.parent match {
      case Some(Node(_: SoftAST.DeclarationAST | _: SoftAST.FunctionSignature, _)) =>
        // The identifier is defined within the group.
        // For example, `Contract MyContract(ident)`
        Iterator.single(
          SourceLocation.NodeSoft(
            ast = identNode.data.code,
            source = sourceCode
          )
        )

      case Some(node) =>
        /*
         * If this is an inheritance group, then disable executing inheritance search.
         * For example, in the following case, the `Parent`s `param` should not be returned.
         * {{{
         *   Contract Parent(param: Type)
         *   Contract Child(>>param<<: Type) extends Parent(para@@m)
         * }}}
         */
        val updatedSettings =
          if (node.isWithinInheritance())
            settings.copy(includeInheritance = false)
          else
            settings

        search(
          identNode = identNode,
          sourceCode = sourceCode,
          cache = cache,
          settings = updatedSettings,
          detectCallSyntax = detectCallSyntax
        )

      case None =>
        search(
          identNode = identNode,
          sourceCode = sourceCode,
          cache = cache,
          settings = settings,
          detectCallSyntax = detectCallSyntax
        )
    }

  /**
   * Expands and searches the given [[SoftAST.MethodCall]].
   *
   * @param methodCallNode The node representing the [[SoftAST.MethodCall]] being searched.
   * @param identNode      The node representing the identifier being searched (i.e. the clicked/selected node).
   * @param sourceCode     The block-part and its source code state where this search is executed.
   * @param cache          The workspace state and its cached trees.
   * @return An iterator over definition search results.
   */
  private def searchMethodCall(
      methodCallNode: Node[SoftAST.MethodCall, SoftAST],
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      cache: WorkspaceSearchCache,
      settings: GoToDefSetting,
      detectCallSyntax: Boolean
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    (methodCallNode.data.leftExpression, methodCallNode.data.rightExpression) match {
      case (left @ (_: SoftAST.ReferenceCall | _: SoftAST.Identifier), _) if left contains identNode =>
        /*
         * A method-call's definitions can be searched without searching its type-information,
         * if the *left* expression of the method-call is being searched, and it's either a reference-call or an identifier.
         *
         * {{{
         *   contra@@ct().function()   // reference-call
         *   contra@@ct.function()     // identifier
         * }}}
         */
        search(
          identNode = identNode,
          sourceCode = sourceCode,
          cache = cache,
          settings = settings,
          detectCallSyntax = detectCallSyntax
        )

      case (left: SoftAST.Identifier, right @ (_: SoftAST.Identifier | _: SoftAST.ReferenceCall)) if right contains identNode =>
        /*
         * This might be a static call.
         *
         * {{{
         *   MyEnum.Valu@@e                // Static call on an Enum
         *   MyContract.encodeFiel@@ds!()    // Static call on a Contract
         * }}}
         */

        // Node of the type name `MyEnum` or `MyContract`
        val leftNode = methodCallNode.findAtIndex(left.index)

        // The identifier node of the right side, which can be an identifier or a reference-call.
        val rightNode =
          methodCallNode
            .findAtIndex(right.index)
            .flatMap {
              node =>
                node.walkDown.collectFirst {
                  case node @ Node(ident: SoftAST.Identifier, _) if right contains ident =>
                    node.upcast(ident)
                }
            }

        (leftNode, rightNode) match {
          case (Some(leftNode @ Node(left: SoftAST.Identifier, _)), Some(rightNode @ Node(right: SoftAST.Identifier, _))) =>
            // This could be a static-call.
            val result =
              searchStaticCallSoft(
                left = leftNode.upcast(left),
                right = rightNode.upcast(right),
                sourceCode = sourceCode,
                cache = cache,
                settings = settings,
                detectCallSyntax = detectCallSyntax
              )

            if (result.nonEmpty)
              result
            else
              // If no result, execute a typed-search which searches strict-AST for the node's type information.
              searchTypeCallStrict(
                theType = left,
                typeProperty = identNode,
                sourceCode = sourceCode,
                cache = cache,
                settings = settings,
                detectCallSyntax = detectCallSyntax
              )

          case (leftNode, rightNode) =>
            // Unlikely to occur: If either is empty, then this is due to a bug searching for the identifier nodes. Log it for debugging.
            if (leftNode.zip(rightNode).isEmpty)
              logger.error(s"Not found: Static-call Node info. left: ${leftNode.map(_.toCode())}. right: ${rightNode.map(_.toCode())}. FileURI: ${sourceCode.parsed.fileURI}")

            searchTypeCallStrict(
              theType = left,
              typeProperty = identNode,
              sourceCode = sourceCode,
              cache = cache,
              settings = settings,
              detectCallSyntax = detectCallSyntax
            )
        }

      case (SoftAST.ReferenceCall(_, ident: SoftAST.Identifier, _, _, _, _), right) if right contains identNode =>
        // This is a chained method call, where the left side is a reference call.
        // For example: `one().two()`
        //               - `two()` is the identNode
        //               - `one()` is the left reference call.
        // To process this, fetch the return type of `one()` and search for the `identNode` in the returned typed.
        searchTypeCallStrict(
          theType = ident,
          typeProperty = identNode,
          sourceCode = sourceCode,
          cache = cache,
          settings = settings,
          detectCallSyntax = detectCallSyntax
        )

      case (SoftAST.MethodCall(_, _, _, _, _, leftExpressionsRightSide: SoftAST.ReferenceCallOrIdentifier, _, _), right) if right contains identNode =>
        // This is a chained method call, where the left side is another method call.
        // For example: `one().two().three()`
        //               - `three()` is the identNode
        //               - `one().two()` is the left method call.
        // To process this, fetch the return type of `two()`, and search for the `identNode` in the returned typed.
        leftExpressionsRightSide.identifier match {
          case theType: SoftAST.Identifier =>
            searchTypeCallStrict(
              theType = theType,
              typeProperty = identNode,
              sourceCode = sourceCode,
              cache = cache,
              settings = settings,
              detectCallSyntax = detectCallSyntax
            )

          case _: SoftAST.IdentifierExpected =>
            Iterator.empty
        }

      case _ =>
        // To avoid incorrect renaming results - do not execute `searchStaticCallSoft` for tree hierarchy that is unknown.
        Iterator.empty
    }

  /**
   * Performs a static call search.
   *
   * Examples:
   * {{{
   *   MyEnum.Value
   *   MyContract.encodeFields!(...)
   * }}}
   *
   * @param left             Left-hand side of the static call expression.
   * @param right            Right-hand side of the static call expression.
   * @param sourceCode       The source-code where this search was executed.
   * @param cache            Workspace state and its cached trees.
   * @param detectCallSyntax If `true`, attemtps to match call-like syntax.
   * @return Matched source-locations.
   */
  private def searchStaticCallSoft(
      left: Node[SoftAST.Identifier, SoftAST],
      right: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      cache: WorkspaceSearchCache,
      settings: GoToDefSetting,
      detectCallSyntax: Boolean
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] = {
    val typeDef =
      searchTypeDefinitionSoft(
        typeIdentifier = left,
        sourceCode = sourceCode,
        cache = cache,
        settings = settings,
        detectCallSyntax = detectCallSyntax
      )

    typeDef flatMap {
      // Static calls are only evaluated for `Enum`s and `Contract`s.
      case SourceLocation.NodeSoft(definition @ (_: SoftAST.Enum | _: SoftAST.Template), source) =>
        val virtualNode =
          buildVirtualNode(
            target = right,
            sourceIndex = point(definition.index.to)
          )

        virtualNode match {
          case Some(virtualNode) =>
            search(
              identNode = virtualNode,
              sourceCode =
                // Search within this found definition.
                SourceLocation.CodeSoft(
                  part = definition,
                  parsed = source.parsed
                ),
              cache = cache,
              settings = settings,
              detectCallSyntax = detectCallSyntax
            )

          case None =>
            logger.error(s"Error: Virtual `Identifier` not found. TypeId: ${definition.toCode()}. FileURI ${source.parsed.fileURI}")
            Iterator.empty
        }

      case _ =>
        logger.trace(s"Not a static call: ${left.toCode()}.${right.toCode()}")
        Iterator.empty
    }
  }

  /**
   * Performs a basic type-def search on [[SoftAST]].
   * Use this search only if the exact type-name/identifier is known.
   *
   * {{{
   *   MyEnu@@m.Value               // Can handle this
   *   MyContr@@act.encodeFields()  // Can handle this
   *
   *   fn function(contract: MyContract) -> () {
   *     // Cannot handle this. Requires type-inference which is handled by [[searchTypeCallStrict]].
   *     contra@@ct.encodeFields()
   *   }
   * }}}
   *
   * @param typeIdentifier   The reference identifier being searched.
   * @param sourceCode       The source code where this search was executed.
   * @param cache            The workspace state and its cached trees.
   * @param detectCallSyntax If `true`, detects the type of search based on the syntax.
   * @return Type definitions matching the given type identifier.
   */
  private def searchTypeDefinitionSoft(
      typeIdentifier: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      cache: WorkspaceSearchCache,
      settings: GoToDefSetting,
      detectCallSyntax: Boolean
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeSoft[SoftAST.TypeDefinitionAST]] = {
    val definitions =
      search(
        identNode = typeIdentifier,
        sourceCode = sourceCode,
        cache = cache,
        settings = settings,
        detectCallSyntax = detectCallSyntax
      )

    definitions flatMap {
      definition =>
        // Lift the type-name `CodeString` of each type-def to concrete `TypeDeclaration`.
        definition.source.part.toNode.findAtIndex(definition.ast.index) match {
          case Some(node) =>
            // Single parent check only because SoftAST only does static type search (e.g. `MyContract.function`).
            // So the identifier must be within a subtype of `TypeDefinitionAST`.
            node.parent.collectFirst {
              case Node(typeDef: SoftAST.TypeDefinitionAST, _) if typeDef contains definition.ast =>
                // Lift the type from `CodeString` to `TypeDefinitionAST`.
                definition.copy(ast = typeDef)
            }

          case None =>
            logger.error(s"Node not found for definition: ${definition.ast.text}. Index: ${definition.ast.index}. FileURI: ${definition.source.parsed.fileURI}")
            Iterator.empty
        }
    }
  }

  /**
   * Searches for a property within the given type.
   *
   * For example, in the following code:
   *  - `myContract` is the type.
   *  - `function` is a property within that type.
   * {{{
   *   myContract.function()
   *   myContract.function
   * }}}
   *
   * @param theType      The type expression to search within.
   * @param typeProperty The identifier node representing the property, this is the selected or clicked node.
   * @param sourceCode   The block-part and its source code state where this search is executed.
   * @param cache        The workspace state and its cached trees.
   * @return An iterator matching properties found in the given type.
   */
  private def searchTypeCallStrict(
      theType: SoftAST.Identifier,
      typeProperty: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      cache: WorkspaceSearchCache,
      settings: GoToDefSetting,
      detectCallSyntax: Boolean
    )(implicit searchCache: SearchCache,
      logger: ClientLogger): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] = {
    // Find all the type definitions.
    val typeDefs =
      CodeProvider
        .goToTypeDef
        .search(
          linePosition = theType.index.toLineRange(sourceCode.parsed.code).from,
          fileURI = sourceCode.parsed.fileURI,
          workspace = cache.workspace,
          searchSettings = ()
        )

    typeDefs match {
      case Some(Right(typeDefs)) =>
        // TODO: Execute in parallel
        typeDefs flatMap {
          typDef =>
            // Type-defs come from ralph-compiler's AST, which is of type `CodeStrict`, convert it to `CodeSoft`.
            typDef.source.toCodeSoft() match {
              case Some(softTypeDef) =>
                // create a virtual node at the end of the tree.
                val virtualNode =
                  buildVirtualNodeAtSourceEnd(
                    target = typeProperty,
                    source = softTypeDef
                  )

                virtualNode match {
                  case Some(virtualNode) =>
                    // Search within the type-definition, with the virtual node as the target.
                    search(
                      identNode = virtualNode,
                      sourceCode = softTypeDef,
                      cache = cache,
                      settings = settings,
                      detectCallSyntax = detectCallSyntax
                    )

                  case None =>
                    logger.error(s"Error: Virtual `Identifier` not found. TypeId: ${typDef.ast}. FileURI ${typDef.source.parsed.fileURI}")
                    Iterator.empty
                }

              case None =>
                logger.error(s"Type-definition could not be converted to SoftAST. TypeId: ${typDef.ast}. FileURI ${typDef.source.parsed.fileURI}")
                Iterator.empty
            }
        }

      case Some(Left(error)) =>
        logger.error(s"Error searching type-definition for '${typeProperty.data.code.text}'. Reason: ${error.message}. FileURI: ${sourceCode.parsed.fileURI}")
        Iterator.empty

      case None =>
        logger.info(s"No type-definitions found for '${typeProperty.data.code.text}'. FileURI: ${sourceCode.parsed.fileURI}")
        Iterator.empty
    }
  }

  /**
   * Given a collection of expressions, expands each expression and searches within it for all possible definitions.
   *
   * @param expressions The expressions to expand and search.
   * @param target      The identifier being searched.
   * @param sourceCode  The source code state where this search is executed.
   * @return An iterator over the locations of the definitions.
   */
  private def searchExpressions(
      expressions: Iterable[SoftAST.ExpressionAST],
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    expressions
      .iterator
      .flatMap {
        expression =>
          searchExpression(
            expression = expression,
            target = target,
            sourceCode = sourceCode
          )
      }

  /**
   * Expands the given identifier and matches its name against a target.
   *
   * @param identifier The identifier to expand and match.
   * @param target     The identifier being searched.
   * @param sourceCode The source code state where this search is executed.
   * @return An iterator over the locations of the definitions.
   */
  private def searchIdentifier(
      identifier: SoftAST.IdentifierAST,
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    identifier match {
      case identifier: SoftAST.Identifier =>
        searchExpression(
          expression = identifier,
          target = target,
          sourceCode = sourceCode
        )

      case _: SoftAST.IdentifierExpected =>
        Iterator.empty
    }

  /**
   * Given an expression, expands the expression and searches within it for all possible definitions.
   *
   * @param expression The expression to expand and search.
   * @param target     The identifier being searched.
   * @param sourceCode The source code state where this search is executed.
   * @return An iterator over the locations of the definitions.
   */
  @tailrec
  private def searchExpression(
      expression: SoftAST.ExpressionAST,
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    expression match {
      case ast: SoftAST.VariableDeclaration =>
        // expand variable declaration and search within the assignment
        searchExpression(
          expression = ast.assignment,
          target = target,
          sourceCode = sourceCode
        )

      case ast: SoftAST.TypeAssignment =>
        // expand type assigment and search within the left expression
        searchExpression(
          expression = ast.expressionLeft,
          target = target,
          sourceCode = sourceCode
        )

      case map: SoftAST.MapAssignment =>
        // expand mapping assignment and search within the identifier
        searchIdentifier(
          identifier = map.identifier,
          target = target,
          sourceCode = sourceCode
        )

      case group: SoftAST.Group[_, _, _] =>
        // Expand the group and search the expressions within
        searchExpressions(
          expressions = group.expressions,
          target = target,
          sourceCode = sourceCode
        )

      case assignment: SoftAST.Assignment =>
        // Expand the expression within this assignment and search within
        searchExpression(
          expression = assignment.expressionLeft,
          target = target,
          sourceCode = sourceCode
        )

      case binding: SoftAST.MutableBinding =>
        // Search the identifier
        searchIdentifier(
          identifier = binding.identifier,
          target = target,
          sourceCode = sourceCode
        )

      case identifier: SoftAST.Identifier if identifier.code.text == target.data.code.text =>
        // Check if the identifier matches the text in the selected `identNode`.
        Iterator.single(
          SourceLocation.NodeSoft(
            ast = identifier.code,
            source = sourceCode
          )
        )

      case _ =>
        Iterator.empty
    }

}
