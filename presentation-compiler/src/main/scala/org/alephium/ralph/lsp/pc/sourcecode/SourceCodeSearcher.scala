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

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.protocol.vm.StatefulContext
import org.alephium.ralph.{Type, Ast}
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.sourcecode.error._

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/**
 * Search functions related to [[SourceCodeState]]
 */
object SourceCodeSearcher {

  /**
   * Find the parsed state ([[SourceCodeState.Parsed]]) for the given file URI.
   *
   * @param fileURI    The file URI of the parsed source-code.
   * @param sourceCode The source code files to search.
   * @return - Right: If the parsed state was found.
   *         - Left: An error, if the source-code is not in a parsed state.
   */
  def findParsed(
      fileURI: URI,
      sourceCode: ArraySeq[SourceCodeState]): Either[CompilerMessage.Error, SourceCodeState.Parsed] =
    sourceCode.find(_.fileURI == fileURI) match {
      case Some(source) =>
        source match {
          case _: SourceCodeState.OnDisk | _: SourceCodeState.UnCompiled =>
            Left(SourceCodeIsNotCompiled(fileURI))

          case _: SourceCodeState.ErrorAccess =>
            Left(SourceCodeAccessFailed(fileURI))

          case parsed: SourceCodeState.Parsed =>
            Right(parsed)

          case compiled: SourceCodeState.Compiled =>
            Right(compiled.parsed)

          case _: SourceCodeState.ErrorParser =>
            Left(SourceCodeHasParserErrors(fileURI))

          case errored: SourceCodeState.ErrorCompilation =>
            Right(errored.parsed)
        }

      case None =>
        Left(SourceCodeNotFound(fileURI))
    }

  /**
   * Collects all source files with valid parsed syntax.
   *
   * @param sourceCode The source files to filter.
   * @return An array sequence containing parsed source code files.
   */
  def collectParsed(sourceCode: ArraySeq[SourceCodeState]): ArraySeq[SourceCodeState.Parsed] =
    sourceCode collect {
      case parsed: SourceCodeState.Parsed =>
        parsed

      case compiled: SourceCodeState.Compiled =>
        compiled.parsed

      case errored: SourceCodeState.ErrorCompilation =>
        errored.parsed
    }

  /**
   * Collect all unique import statements from source code.
   *
   * @param sourceCode Source code to search import statements within.
   * @return All import statements.
   */
  def collectImportStatements(sourceCode: ArraySeq[SourceCodeState.Parsed]): ArraySeq[Tree.Import] =
    sourceCode
      .flatMap {
        parsed =>
          parsed.ast.statements.collect {
            case imported: Tree.Import =>
              imported
          }
      }
      .distinctBy(_.string.value)

  /**
   * Collects unique inherited parents for all input parsed files and for each tree within a file.
   *
   * @param sourceCode The source code to find inherited parents for.
   * @param workspace  The source code containing the parents.
   * @return All inherited parent implementations and their source files.
   */
  def collectInheritedParentsForAll(
      sourceCode: ArraySeq[SourceCodeState.Parsed],
      workspace: ArraySeq[SourceCodeState.Parsed]): ArraySeq[SourceLocation.Code] = {
    val workspaceTrees = collectSourceTrees(workspace)

    val parents =
      sourceCode flatMap {
        parsed =>
          collectInheritedParentsForAll(
            sourceCode = parsed,
            workspace = workspaceTrees
          )
      }

    // unique parents
    parents.distinct
  }

  /**
   * Collects unique inherited parents of each tree within a parsed file.
   *
   * @param sourceCode The source code to find inherited parents for.
   * @param workspace  The source code containing the parents.
   * @return All inherited parent implementations and their source files.
   */
  def collectInheritedParentsForAllTrees(
      sourceCode: SourceCodeState.Parsed,
      workspace: ArraySeq[SourceCodeState.Parsed]): Seq[SourceLocation.Code] =
    collectInheritedParentsForAll(
      sourceCode = sourceCode,
      workspace = collectSourceTrees(workspace)
    ).distinct

  /**
   * Collects unique inherited parents of each tree within a parsed file.
   *
   * @param sourceCode The source code to find inherited parents for.
   * @param workspace  The source trees containing the parents.
   * @return All inherited parent implementations and their source files.
   */
  def collectInheritedParentsForAll(
      sourceCode: SourceCodeState.Parsed,
      workspace: ArraySeq[SourceLocation.Code]): Seq[SourceLocation.Code] =
    collectInheritedParents(
      source = collectSourceTrees(sourceCode).to(ArraySeq),
      allSource = workspace
    ).distinct

  /**
   * Collects all trees within each parsed source file.
   *
   * @param sourceCode The parsed source files to process.
   * @return An sequence of source-tree and its parsed source-file mappings.
   */
  def collectSourceTrees(
      sourceCode: ArraySeq[SourceCodeState.Parsed]): ArraySeq[SourceLocation.Code] =
    sourceCode flatMap collectSourceTrees

  /**
   * Collects all trees within a parsed source file.
   *
   * @param sourceCode The parsed source file to process.
   * @return An sequence of source-tree and the parsed source-file mappings.
   */
  def collectSourceTrees(
      sourceCode: SourceCodeState.Parsed): Seq[SourceLocation.Code] =
    sourceCode.ast.statements.collect {
      case tree: Tree.Source =>
        SourceLocation.Code(
          tree = tree,
          parsed = sourceCode
        )
    }

  /**
   * Collects all source tree locations for the given types.
   *
   * @param types           The types of trees for which to collect source tree locations.
   * @param workspaceSource All workspace trees.
   * @return An array sequence containing all source tree locations.
   */
  def collectTypes(
      types: Seq[Type],
      workspaceSource: ArraySeq[SourceLocation.Code]): ArraySeq[SourceLocation.Code] =
    workspaceSource flatMap {
      code =>
        collectTypes(
          types = types,
          code = code
        )
    }

  /**
   * Collects all source trees for the given types within the provided code.
   *
   * @param types The types for which to collect source trees.
   * @param code  The source code to search within.
   * @return A sequence containing all matching source trees.
   */
  def collectTypes(
      types: Seq[Type],
      code: SourceLocation.Code): Seq[SourceLocation.Code] =
    // collect all trees with matching types
    types collect {
      case Type.NamedType(id) if code.tree.typeId() == id =>
        code

      case Type.Struct(id) if code.tree.typeId() == id =>
        code

      case Type.Contract(id) if code.tree.typeId() == id =>
        code
    }

  /**
   * Collects all functions from trees with the given types.
   *
   * @param types           The types of trees from which to collect functions.
   * @param workspaceSource All workspace trees.
   * @return An iterator containing all function implementations.
   */
  def collectFunctions(
      types: Seq[Type],
      workspaceSource: ArraySeq[SourceLocation.Code]): Iterator[SourceLocation.Node[Ast.FuncDef[StatefulContext]]] = {
    // collect all trees with matching types
    val treesOfMatchingTypes =
      collectTypes(
        types = types,
        workspaceSource = workspaceSource
      )

    // perform search on only the matching types
    collectFunctions(
      trees = treesOfMatchingTypes,
      workspaceSource = workspaceSource
    )
  }

  /**
   * Collects all functions within the given trees.
   *
   * @param trees           The trees from which to collect functions.
   * @param workspaceSource All workspace trees.
   * @return An iterator containing all function implementations.
   */
  def collectFunctions(
      trees: ArraySeq[SourceLocation.Code],
      workspaceSource: ArraySeq[SourceLocation.Code]): Iterator[SourceLocation.Node[Ast.FuncDef[StatefulContext]]] =
    trees
      .iterator
      .flatMap {
        code =>
          collectFunctions(
            tree = code,
            workspaceSource = workspaceSource
          )
      }

  /**
   * Collects all functions within the given tree.
   *
   * @param tree            The tree from which to collect functions.
   * @param workspaceSource All workspace trees.
   * @return An iterator containing all function implementations.
   */
  def collectFunctions(
      tree: SourceLocation.Code,
      workspaceSource: ArraySeq[SourceLocation.Code]): Iterator[SourceLocation.Node[Ast.FuncDef[StatefulContext]]] = {
    // the function could be within a nested parent, collect all parents.
    val parents =
      collectInheritedParents(
        source = tree,
        allSource = workspaceSource
      )

    // all code to search, include the current code and its parents.
    val allCode = tree +: parents

    // Search for function IDs that match.
    allCode.iterator.flatMap {
      code =>
        code.tree.ast match {
          case Left(contract) =>
            contract
              .funcs
              .map {
                funcDef =>
                  SourceLocation.Node(
                    ast = funcDef,
                    source = code
                  )
              }

          case Right(_) =>
            Iterator.empty
        }
    }
  }

  /**
   * Collects all function definitions from the provided source code.
   *
   * @param sourceCode The source code from which to collect function definitions.
   * @return           An iterator containing all function implementations.
   */
  def collectFunctions(sourceCode: SourceLocation.Code): Iterator[SourceLocation.Node[Ast.FuncDef[StatefulContext]]] =
    // TODO: Improve selection by checking function argument count and types.
    sourceCode.tree.ast match {
      case Left(ast) =>
        ast
          .funcs
          .iterator
          .map {
            funcDef =>
              SourceLocation.Node(
                ast = funcDef,
                source = sourceCode
              )
          }

      case Right(_) =>
        Iterator.empty
    }

  /**
   * Collects all function definitions from the provided parsed source code.
   *
   * @param source The parsed source code from which to collect function definitions.
   * @return An iterator containing all function implementations.
   */
  def collectFunctions(source: SourceCodeState.Parsed): Iterator[SourceLocation.Node[Ast.FuncDef[StatefulContext]]] =
    source
      .ast
      .statements
      .iterator
      .flatMap {
        case tree: Tree.Source =>
          // search for the matching functionIds within the built-in source file.
          collectFunctions(SourceLocation.Code(tree, source))

        case _: Tree.Import =>
          Iterator.empty
      }

  /**
   * Collects all parent source implementations inherited by the given
   * source tree within the provided source code files.
   *
   * @param source    The source tree to search for parent implementations.
   * @param allSource The source code files containing the parent implementations.
   * @return All parent source implementations found.
   */
  def collectInheritedParents(
      source: ArraySeq[SourceLocation.Code],
      allSource: ArraySeq[SourceLocation.Code]): Seq[SourceLocation.Code] =
    source.flatMap {
      source =>
        collectInheritedParents(
          source = source,
          allSource = allSource
        )
    }.distinct

  /**
   * Collects all parent source implementations inherited by the given
   * source tree within the provided source code files.
   *
   * @param source    The source tree to search for parent implementations.
   * @param allSource The source code files containing the parent implementations.
   * @return All parent source implementations found.
   */
  def collectInheritedParents(
      source: SourceLocation.Code,
      allSource: ArraySeq[SourceLocation.Code]): Seq[SourceLocation.Code] =
    source.tree.ast match {
      case Left(contract) =>
        collectInheritedParents(
          inheritances = contract.inheritances,
          allSource = allSource,
          processedTrees = mutable.Set(source)
        )

      case Right(_) =>
        Seq.empty
    }

  /**
   * Collects all children implementing or extending the given
   * source tree within the provided source code files.
   *
   * @param source    The source tree to search for child implementations.
   * @param allSource The source code files containing the child implementations.
   * @return All child trees along with their corresponding source files.
   */
  def collectImplementingChildren(
      source: SourceLocation.Code,
      allSource: ArraySeq[SourceLocation.Code]): Seq[SourceLocation.Code] =
    source.tree.ast match {
      case Left(contract) =>
        collectImplementingChildren(
          contract = contract,
          allSource = allSource,
          processedTrees = mutable.Set(source)
        )

      case Right(_) =>
        Seq.empty
    }

  /**
   * Collects all source-trees representing implementations of the provided inheritances.
   *
   * @param inheritances   The inheritances to search for.
   * @param allSource      The source code files containing the inheritance implementations.
   * @param processedTrees A buffer to store processed source trees to avoid duplicate processing.
   *                       This is a mutable collection, so this function must be private.
   * @return All inheritance implementations along with their corresponding source files.
   */
  private def collectInheritedParents(
      inheritances: Seq[Ast.Inheritance],
      allSource: ArraySeq[SourceLocation.Code],
      processedTrees: mutable.Set[SourceLocation.Code]): Seq[SourceLocation.Code] =
    allSource flatMap {
      source =>
        // collect the trees that belong to one of the inheritances and the ones that are not already processed
        if (inheritances.exists(_.parentId == source.tree.typeId()) && !processedTrees.contains(source)) {
          processedTrees addOne source

          source.tree.ast match {
            case Left(contract) =>
              // TODO: There might a need for this to be tail-recursive to avoid stackoverflow on very large codebases.
              val parents =
                collectInheritedParents(
                  inheritances = contract.inheritances,
                  allSource = allSource,
                  processedTrees = processedTrees
                )

              parents :+ source

            case Right(_) =>
              Seq.empty
          }

        } else {
          Seq.empty
        }
    }

  /**
   * Collects all source-trees representing children that implement or extend the given contract.
   *
   * @param contract       The contract for which its children are being searched.
   * @param allSource      The source code files containing the inheritances.
   * @param processedTrees A buffer to store processed source trees to avoid duplicate processing.
   *                       This is a mutable collection, so this function must be private.
   * @return All child trees along with their corresponding source files.
   */
  private def collectImplementingChildren(
      contract: Ast.ContractWithState,
      allSource: ArraySeq[SourceLocation.Code],
      processedTrees: mutable.Set[SourceLocation.Code]): Seq[SourceLocation.Code] =
    allSource flatMap {
      source =>
        // collect the trees that belong to one of the inheritances and the ones that are not already processed
        if (source.tree.ast.left.exists(_.inheritances.exists(_.parentId == contract.ident)) && !processedTrees.contains(source)) {
          processedTrees addOne source

          source.tree.ast match {
            case Left(contract) =>
              // TODO: There might a need for this to be tail-recursive to avoid stackoverflow on very large codebases.
              val children =
                collectImplementingChildren(
                  contract = contract,
                  allSource = allSource,
                  processedTrees = processedTrees
                )

              children :+ source

            case Right(_) =>
              Seq.empty
          }
        } else {
          Seq.empty
        }
    }

}
