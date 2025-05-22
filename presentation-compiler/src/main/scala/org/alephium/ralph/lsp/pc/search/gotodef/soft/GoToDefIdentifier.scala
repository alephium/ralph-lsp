// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef.soft

import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.gotodef.{GoToDefSetting, ScopeWalker}
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeSearcher, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceSearcher, WorkspaceState}
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.pc.workspace.build.BuildState

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
    )(implicit logger: ClientLogger): Iterator[SourceLocation.GoToDefSoft] =
    searchParent(
      identNode = identNode,
      parent = identNode.parent,
      sourceCode = sourceCode,
      workspace = workspace,
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
   * @param workspace  The workspace state where the source-code is located.
   * @return An iterator over definition search results.
   */
  private def searchParent(
      identNode: Node[SoftAST.Identifier, SoftAST],
      parent: Option[Node[SoftAST, SoftAST]],
      sourceCode: SourceLocation.CodeSoft,
      workspace: WorkspaceState.IsSourceAware,
      settings: GoToDefSetting
    )(implicit logger: ClientLogger): Iterator[SourceLocation.GoToDefSoft] = {
    @inline def runFullSearch() =
      search(
        identNode = identNode,
        sourceCode = sourceCode,
        workspace = workspace,
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
      case Some(node @ Node(_: SoftAST.ReferenceCall, _)) =>
        node.parent match {
          case Some(Node(_: SoftAST.DotCall, _)) =>
            // TODO - This needs type information from Strict-AST.
            Iterator.empty

          case _ =>
            runFullSearch()
        }

      case Some(Node(_: SoftAST.DotCall, _)) =>
        // TODO - This needs type information from Strict-AST.
        Iterator.empty

      case Some(node @ Node(assignment: SoftAST.Assignment, _)) if assignment.expressionLeft == identNode.data =>
        node.parent match {
          // If it's an assignment, it must also be a variable declaration for the current node to be a self.
          case Some(Node(_: SoftAST.VariableDeclaration | _: SoftAST.Const, _)) =>
            self()

          case _ =>
            // invoke full scope search.
            runFullSearch()
        }

      case Some(Node(assignment: SoftAST.TypeAssignment, _)) if assignment.expressionLeft == identNode.data =>
        self()

      case Some(Node(assignment: SoftAST.MutableBinding, _)) if assignment.identifier == identNode.data =>
        self()

      case Some(Node(function: SoftAST.FunctionSignature, _)) if function.fnName == identNode.data =>
        self()

      case Some(Node(template: SoftAST.Template, _)) if template.identifier == identNode.data =>
        self()

      case Some(Node(enumAST: SoftAST.Enum, _)) if enumAST.identifier == identNode.data =>
        self()

      case Some(Node(event: SoftAST.Event, _)) if event.identifier == identNode.data =>
        self()

      case Some(Node(struct: SoftAST.Struct, _)) if struct.identifier == identNode.data =>
        self()

      case Some(node @ Node(group: SoftAST.Group[_, _], _)) =>
        searchGroup(
          group = node.upcast(group),
          identNode = identNode,
          sourceCode = sourceCode,
          workspace = workspace,
          settings = settings,
          detectCallSyntax = true
        )

      case Some(node @ Node(tail: SoftAST.GroupTail, _)) =>
        node.parent match {
          case Some(node @ Node(group: SoftAST.Group[_, _], _)) => // a `GroupTail` is always contained with a `Group`
            searchGroup(
              group = node.upcast(group),
              identNode = identNode,
              sourceCode = sourceCode,
              workspace = workspace,
              settings = settings,
              detectCallSyntax = true
            )

          case _ =>
            logger.trace(s"GroupTail not contained with a group. Index: ${tail.index}. File: ${sourceCode.parsed.fileURI}")
            runFullSearch()
        }

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
  private def search(
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      workspace: WorkspaceState.IsSourceAware,
      settings: GoToDefSetting,
      detectCallSyntax: Boolean
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] = {
    val allTrees =
      WorkspaceSearcher.collectAllTreesSoft(workspace)

    def runSearch(detectCallSyntax: Boolean) = {
      val inherited =
        if (settings.includeInheritance)
          searchInheritance(
            target = identNode,
            sourceCode = sourceCode,
            build = workspace.build,
            workspaceTrees = allTrees,
            detectCallSyntax = detectCallSyntax
          )
        else
          Iterator.empty

      val local =
        searchLocal(
          from = sourceCode.part.toNode,
          target = identNode,
          sourceCode = sourceCode,
          detectCallSyntax = detectCallSyntax
        )

      val globals =
        searchGlobal(
          target = identNode,
          trees = allTrees.iterator,
          detectCallSyntax = detectCallSyntax
        )

      (local.iterator ++ inherited ++ globals).distinct
    }

    val result = runSearch(detectCallSyntax)

    if (detectCallSyntax && result.isEmpty)
      runSearch(detectCallSyntax = false) // The restricted exact call syntax search returned no results. Executing a relaxed search.
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
   * @param from             The position from where the search should begin.
   * @param target           The identifier being searched.
   * @param sourceCode       The source code state where the `from` node is located.
   * @param detectCallSyntax If `true`, ensures that when a function is called,
   *                         the search is restricted to reference calls only and does not
   *                         return variables with the same name.
   * @return An iterator over the locations of the definitions.
   */
  private def searchLocal(
      from: Node[SoftAST, SoftAST],
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      detectCallSyntax: Boolean): Iterable[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    // Reference calls are then ones ending with parentheses, for example `refCall()`.
    // Reference calls should only search for function and contract calls, not variables.
    ScopeWalker.walk(
      from = from,
      anchor = target.data.index
    ) {
      case Node(variable: SoftAST.VariableDeclaration, _) if !detectCallSyntax || (!target.isReferenceCall() && !target.isWithinEmit()) =>
        searchExpression(
          expression = variable,
          target = target,
          sourceCode = sourceCode
        )

      case Node(assignment: SoftAST.TypeAssignment, _) if !detectCallSyntax || (!target.isReferenceCall() && !target.isWithinEmit()) =>
        searchExpression(
          expression = assignment,
          target = target,
          sourceCode = sourceCode
        )

      case Node(binding: SoftAST.MutableBinding, _) if !detectCallSyntax || (!target.isReferenceCall() && !target.isWithinEmit()) =>
        searchExpression(
          expression = binding,
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
    }

  /**
   * Given a target identifier, searches all inherited contracts for all possible definitions, including
   * the built-in interfaces.
   *
   * @param target           The identifier being searched.
   * @param sourceCode       The source code state where the `target` node is located.
   * @param build            Current workspace build.
   * @param workspaceTrees   All workspace trees in scope, including the imported code.
   * @param detectCallSyntax If `true`, ensures that when a function is called,
   *                         the search is restricted to reference calls only and does not
   *                         return variables with the same name.
   * @return An iterator over the locations of the definitions.
   */
  private def searchInheritance(
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      build: BuildState.Compiled,
      workspaceTrees: ArraySeq[SourceLocation.CodeSoft],
      detectCallSyntax: Boolean
    )(implicit logger: ClientLogger): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] = {
    // The actual inherited code as defined in the AST
    val inheritedTrees =
      SourceCodeSearcher.collectInheritedParents(
        source = sourceCode,
        allSource = workspaceTrees
      )

    // Inheritance search must include all built-in interfaces.
    val builtInTrees =
      WorkspaceSearcher.collectAllDependencyTreesSoft(
        dependencyID = DependencyID.BuiltIn,
        build = build
      ) match {
        case Some((builtIn, builtInTrees)) =>
          // Primitives should not be included within inheritance search.
          builtInTrees.filter(!_.parsed.isPrimitive(builtIn))

        case None =>
          Iterator.empty
      }

    // Merge both the actual inherited trees and the built-in trees.
    val allInheritedTrees =
      inheritedTrees ++ builtInTrees

    // Execute inheritance search.
    searchInheritance(
      target = target,
      inheritance = allInheritedTrees.iterator,
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
      buildVirtualNode(
        target = target,
        sourceIndex = point(inherited.part.index.to)
      )

    virtualNode match {
      case Some(virtualNode) =>
        // execute a local search within the inherited source code
        searchLocal(
          from = inherited.part.toNode,
          target = virtualNode,
          sourceCode = inherited,
          detectCallSyntax = detectCallSyntax
        )

      case None =>
        // This should not occur if the above AST has an identifier.
        logger.error(s"Error: Virtual `Identifier` not found. File: ${inherited.parsed.fileURI}")
        Iterable.empty
    }
  }

  /**
   * Note: This function is used for searching within inheritance.
   * For details see the documentation of the function [[searchInheritance]].
   *
   * Builds a virtual AST node for the given [[SoftAST.Identifier]] tree,
   * assigning the specified [[SourceIndex]] to a maximum of two parents
   * with the same [[SourceIndex]].
   *
   * A virtual node is created only for the following syntax:
   * - [[SoftAST.Identifier]] — e.g. `Transfer`
   * - [[SoftAST.ReferenceCall]] — e.g. `Transfer()`
   * - [[SoftAST.MethodCall]] — e.g. `Transfer.function`
   * - [[SoftAST.Emit]] — e.g. `emit <<any of the above three cases>>`
   *
   * @param target      The identifier tree to create the virtual node for.
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
            case Some(Node(exp @ (_: SoftAST.ReferenceCall), _)) =>
              // If the identifier is wrapped around a reference call, deep-copy the reference call.
              exp.deepCopy(sourceIndex)

            case _ =>
              // Otherwise deep copy the identifier.
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
            if (detectCallSyntax && (target.isReferenceCall() || target.isWithinEmit()))
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
              detectCallSyntax = detectCallSyntax
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
            detectCallSyntax = detectCallSyntax
          )

        case _ =>
          Iterator.empty
      }

    // Check if the name matches the identifier.
    val nameMatches =
      if (!detectCallSyntax || (!target.isReferenceCall() && !target.isWithinEmit()))
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
   * @param detectCallSyntax If `true`, ensures that when a function is called,
   *                         the search is restricted to reference calls only and does not
   *                         return variables with the same name.
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
        if (detectCallSyntax && (target.isReferenceCall() || target.isWithinEmit()))
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
            detectCallSyntax = detectCallSyntax
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
   * @param detectCallSyntax   If `true`, restricts the search to `event` only when an `emit` is defined.
   * @return An iterator over the locations of the definitions.
   */
  private def searchTemplateIdentifier(
      templateIdentifier: SoftAST.IdentifierAST,
      target: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      detectCallSyntax: Boolean): Iterator[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    // Check if the name matches the identifier.
    if (!detectCallSyntax || ((target.isReferenceCall() || target.isMethodCall()) && !target.isWithinEmit()))
      searchIdentifier(
        identifier = templateIdentifier.identifier,
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
    if (!detectCallSyntax || target.isWithinEmit())
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
    if (!detectCallSyntax || (!target.isReferenceCall() && !target.isWithinEmit() && !target.isMethodCall()))
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
      detectCallSyntax: Boolean): Iterable[SourceLocation.NodeSoft[SoftAST.CodeString]] =
    if (block contains target)
      searchLocal(
        from = block.toNode,
        target = target,
        sourceCode = sourceCode,
        detectCallSyntax = detectCallSyntax
      )
    else
      Iterable.empty

  /**
   * Expands and searches the given group.
   *
   * @param group            The node representing the group being searched.
   * @param identNode        The node representing the identifier being searched, which is also a child of the group.
   * @param sourceCode       The block-part and its source code state where this search is executed.
   * @param workspace        The workspace state where the source-code is located.
   * @param detectCallSyntax If `true`, ensures that when a function is called,
   *                         the search is restricted to reference calls only and does not
   *                         return variables with the same name.
   * @return An iterator over definition search results.
   */
  private def searchGroup(
      group: Node[SoftAST.Group[_, _], SoftAST],
      identNode: Node[SoftAST.Identifier, SoftAST],
      sourceCode: SourceLocation.CodeSoft,
      workspace: WorkspaceState.IsSourceAware,
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
          workspace = workspace,
          settings = updatedSettings,
          detectCallSyntax = detectCallSyntax
        )

      case None =>
        search(
          identNode = identNode,
          sourceCode = sourceCode,
          workspace = workspace,
          settings = settings,
          detectCallSyntax = detectCallSyntax
        )
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

      case group: SoftAST.Group[_, _] =>
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
