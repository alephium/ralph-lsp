// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.protocol.vm.StatefulContext
import org.alephium.ralph.{Ast, Type}
import org.alephium.ralph.lsp.access.compiler.ast.{AstExtra, Tree}
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.sourcecode.error._
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

/**
 * Search functions related to [[SourceCodeState]]
 */
object SourceCodeSearcher extends StrictImplicitLogging {

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

  def findIsParsed(
      fileURI: URI,
      sourceCode: ArraySeq[SourceCodeState]): Either[CompilerMessage.Error, SourceCodeState.IsParsed] =
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

          case error: SourceCodeState.ErrorParser =>
            Right(error)

          case errored: SourceCodeState.ErrorCompilation =>
            Right(errored.parsed)
        }

      case None =>
        Left(SourceCodeNotFound(fileURI))
    }

  /**
   * Collects all source files with valid parsed syntax.
   *
   * @param sourceCode The source files to be filtered
   * @return Source files that are successfully parsed.
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
   * Collects all source files that were parsed and may contain syntax errors.
   *
   * @param sourceCode The source files to be filtered.
   * @return Source files that were parsed at least once.
   */
  def collectIsParsed(sourceCode: ArraySeq[SourceCodeState]): ArraySeq[SourceCodeState.IsParsed] =
    sourceCode collect {
      case parsed: SourceCodeState.IsParsed =>
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
          parsed.astStrict.statements.collect {
            case imported: Tree.Import =>
              imported
          }
      }
      .distinctBy(_.string.value)

  /**
   * Collect all unique import statements from source code.
   *
   * @param sourceCode Source code to search import statements within.
   * @return A sequence of tuples, where each tuple contains:
   *          - An import statement AST (`SoftAST.Import`).
   *          - The corresponding import path as a string.
   */
  def collectImportStatementsSoft(sourceCode: ArraySeq[SourceCodeState.IsParsed])(implicit logger: ClientLogger): ArraySeq[(SoftAST.Import, String)] =
    sourceCode
      .flatMap {
        parsed =>
          parsed.astSoft.fetch() match {
            case Left(error) =>
              logger.error(s"SoftParser Error: '${parsed.fileURI}'", error.error)
              Seq.empty

            case Right(root) =>
              root.parts.collect {
                case imported @ SoftAST.Import(_, _, _, Some(path: SoftAST.StringLiteral)) =>
                  (imported, path.text)
              }
          }
      }
      .distinctBy(_._2)

  /**
   * Collects unique inherited parents for all input parsed files and for each tree within a file.
   *
   * @param sourceCode The source code to find inherited parents for.
   * @param workspace  The source code containing the parents.
   * @return All inherited parent implementations and their source files.
   */
  def collectInheritedParentsForAll(
      sourceCode: ArraySeq[SourceCodeState.Parsed],
      workspace: ArraySeq[SourceCodeState.Parsed]): ArraySeq[SourceLocation.CodeStrict] = {
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
   * Collects unique inherited parents for all input parsed files and for each tree within each file.
   *
   * @param sourceCode The source code to find inherited parents for.
   * @param workspace  The source code containing the parents.
   * @return All inherited parent implementations and their source files.
   */
  def collectInheritedParentsForAllSoft(
      sourceCode: ArraySeq[SourceCodeState.IsParsed],
      workspace: ArraySeq[SourceCodeState.IsParsed]
    )(implicit logger: ClientLogger): ArraySeq[SourceLocation.CodeSoft] = {
    val workspaceTrees = collectSourceTreesSoft(workspace)

    val parents =
      sourceCode flatMap {
        parsed =>
          collectInheritedParentsForAllSoft(
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
      workspace: ArraySeq[SourceCodeState.Parsed]): ArraySeq[SourceLocation.CodeStrict] =
    collectInheritedParentsForAll(
      sourceCode = sourceCode,
      workspace = collectSourceTrees(workspace)
    ).distinct

  /**
   * Collects unique inherited parents of each tree within a parsed file.
   *
   * @param sourceCode The source code to find inherited parents for.
   * @param workspace  The source code containing the parents.
   * @return All inherited parent implementations and their source files.
   */
  def collectInheritedParentsForAllTreesSoft(
      sourceCode: SourceCodeState.IsParsed,
      workspace: ArraySeq[SourceCodeState.IsParsed]
    )(implicit logger: ClientLogger): ArraySeq[SourceLocation.CodeSoft] =
    collectInheritedParentsForAllSoft(
      sourceCode = sourceCode,
      workspace = collectSourceTreesSoft(workspace)
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
      workspace: ArraySeq[SourceLocation.CodeStrict]): ArraySeq[SourceLocation.CodeStrict] =
    collectInheritedParents(
      source = collectSourceTrees(sourceCode).to(ArraySeq),
      allSource = workspace
    ).distinct

  /**
   * Collects unique inherited parents of each tree within a parsed file.
   *
   * @param sourceCode The source code to find inherited parents for.
   * @param workspace  The source trees containing the parents.
   * @return All inherited parent implementations and their source files.
   */
  def collectInheritedParentsForAllSoft(
      sourceCode: SourceCodeState.IsParsed,
      workspace: ArraySeq[SourceLocation.CodeSoft]
    )(implicit logger: ClientLogger): ArraySeq[SourceLocation.CodeSoft] =
    collectInheritedParentsSoft(
      source = collectSourceTreesSoft(sourceCode).to(ArraySeq),
      allSource = workspace
    ).distinct

  /**
   * Collects all trees within each parsed source file.
   *
   * @param sourceCode The parsed source files to process.
   * @return An sequence of source-tree and its parsed source-file mappings.
   */
  def collectSourceTrees(
      sourceCode: ArraySeq[SourceCodeState.Parsed]): ArraySeq[SourceLocation.CodeStrict] =
    sourceCode flatMap collectSourceTrees

  /**
   * Collects all trees within each [[SourceCodeState.IsParsed]] source file.
   *
   * @param sourceCode The parsed source files to process.
   * @return An sequence of source-tree and its parsed source-file mappings.
   */
  def collectSourceTreesSoft(
      sourceCode: ArraySeq[SourceCodeState.IsParsed]
    )(implicit logger: ClientLogger): ArraySeq[SourceLocation.CodeSoft] =
    sourceCode flatMap collectSourceTreesSoft

  /**
   * Collects all trees within a parsed source file.
   *
   * @param sourceCode The parsed source file to process.
   * @return A sequence of source-tree and the parsed source-file mappings.
   */
  def collectSourceTrees(
      sourceCode: SourceCodeState.Parsed): Seq[SourceLocation.CodeStrict] =
    sourceCode.astStrict.statements.collect {
      case tree: Tree.Source =>
        SourceLocation.CodeStrict(
          tree = tree,
          parsed = sourceCode
        )
    }

  /**
   * Collects all trees within a [[SourceCodeState.IsParsed]] source file.
   *
   * @param sourceCode The parsed source file to process.
   * @return A sequence of source-tree and the parsed source-file mappings.
   */
  def collectSourceTreesSoft(
      sourceCode: SourceCodeState.IsParsed
    )(implicit logger: ClientLogger): Seq[SourceLocation.CodeSoft] =
    sourceCode.astSoft.fetch() match {
      case Left(error) =>
        // This will not be required here on the SoftAST merge is complete and
        // failed SoftASTs are a part of the SourceCodeState.ErrorParser.
        logger.error(s"Failed to soft-parse '${sourceCode.fileURI}'", error.error)
        Seq.empty

      case Right(block) =>
        block.parts map {
          part =>
            SourceLocation.CodeSoft(
              part = part,
              parsed = sourceCode
            )
        }
    }

  /**
   * Collects all types available in the provided source code.
   *
   * @param workspaceSource The source code to search for types.
   * @return An iterator containing type identifiers.
   */
  def collectTypes(workspaceSource: Iterator[SourceLocation.CodeStrict]): Iterator[SourceLocation.NodeStrict[Ast.TypeId]] =
    workspaceSource flatMap {
      code =>
        code.tree.typeId() map {
          typeId =>
            SourceLocation.NodeStrict(
              ast = typeId,
              source = code
            )
        }
    }

  /**
   * Collects all global constants available in the provided source code.
   *
   * @param workspaceSource The source code to search for.
   * @return An iterator containing global constants.
   */
  def collectGlobalConstants(workspaceSource: Iterator[SourceLocation.CodeStrict]): Iterator[SourceLocation.NodeStrict[Ast.ConstantVarDef[_]]] =
    workspaceSource collect {
      case source @ SourceLocation.CodeStrict(Tree.Source(ast: Ast.ConstantVarDef[_], _), _) =>
        SourceLocation.NodeStrict(
          ast = ast,
          source = source
        )
    }

  /**
   * Collects all source code with global enums available in the provided source code.
   *
   * @param workspaceSource The source code to search for global enums.
   * @return An iterator containing all global enums.
   */
  def collectGlobalEnumsCode(workspaceSource: Iterator[SourceLocation.CodeStrict]): Iterator[SourceLocation.CodeStrict] =
    workspaceSource collect {
      case code @ SourceLocation.CodeStrict(Tree.Source(_: Ast.EnumDef[_], _), _) =>
        code
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
      workspaceSource: ArraySeq[SourceLocation.CodeStrict]): ArraySeq[SourceLocation.CodeStrict] =
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
      code: SourceLocation.CodeStrict): Seq[SourceLocation.CodeStrict] =
    // collect all trees with matching types
    code.tree.typeId() match {
      case Some(typeId) =>
        types collect {
          case Type.NamedType(id) if typeId == id =>
            code

          case Type.Struct(id) if typeId == id =>
            code

          case Type.Contract(id) if typeId == id =>
            code
        }

      case None =>
        Seq.empty
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
      workspaceSource: ArraySeq[SourceLocation.CodeStrict]): Iterator[SourceLocation.NodeStrict[Ast.FuncDef[StatefulContext]]] = {
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
      trees: ArraySeq[SourceLocation.CodeStrict],
      workspaceSource: ArraySeq[SourceLocation.CodeStrict]): Iterator[SourceLocation.NodeStrict[Ast.FuncDef[StatefulContext]]] =
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
      tree: SourceLocation.CodeStrict,
      workspaceSource: ArraySeq[SourceLocation.CodeStrict]): Iterator[SourceLocation.NodeStrict[Ast.FuncDef[StatefulContext]]] = {
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
          case contract: Ast.ContractWithState =>
            contract
              .funcs
              .map {
                funcDef =>
                  SourceLocation.NodeStrict(
                    ast = funcDef,
                    source = code
                  )
              }

          case _: Ast.Struct | _: Ast.EnumDef[_] | _: Ast.ConstantVarDef[_] | _: Ast.AssetScript =>
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
  def collectFunctions(sourceCode: SourceLocation.CodeStrict): Iterator[SourceLocation.NodeStrict[Ast.FuncDef[StatefulContext]]] =
    // TODO: Improve selection by checking function argument count and types.
    sourceCode.tree.ast match {
      case ast: Ast.ContractWithState =>
        ast
          .funcs
          .iterator
          .map {
            funcDef =>
              SourceLocation.NodeStrict(
                ast = funcDef,
                source = sourceCode
              )
          }

      case _: Ast.Struct | _: Ast.EnumDef[_] | _: Ast.ConstantVarDef[_] | _: Ast.AssetScript =>
        Iterator.empty
    }

  /**
   * Collects all function definitions from the provided parsed source code.
   *
   * @param source The parsed source code from which to collect function definitions.
   * @return An iterator containing all function implementations.
   */
  def collectFunctions(source: SourceCodeState.Parsed): Iterator[SourceLocation.NodeStrict[Ast.FuncDef[StatefulContext]]] =
    source
      .astStrict
      .statements
      .iterator
      .flatMap {
        case tree: Tree.Source =>
          // search for the matching functionIds within the built-in source file.
          collectFunctions(SourceLocation.CodeStrict(tree, source))

        case _: Tree.Import =>
          Iterator.empty
      }

  /**
   * Finds a function named `main` if the given source tree is a transaction script [[Ast.TxScript]].
   *
   * @param sourceCode The transaction script that may contain a `main` function.
   * @return Node representing the `main` function of the transaction script, if found, else None.
   */
  def findTxScriptMainFunction(sourceCode: SourceLocation.CodeStrict): Option[Node[Ast.FuncDef[_], Ast.Positioned]] =
    sourceCode.tree.ast match {
      case _: Ast.TxScript =>
        sourceCode.tree.rootNode.walkDown.collectFirst {
          case functionNode @ Node(funcDef: Ast.FuncDef[_], _) if funcDef.name == AstExtra.TX_SCRIPT_MAIN_FUNCTION_NAME =>
            functionNode.upcast(funcDef)
        }

      case _ =>
        None
    }

  /**
   * Collects all parent source implementations inherited by the given
   * source tree within the provided source code files.
   *
   * @param source    The source trees to search for parent implementations.
   * @param allSource The source trees containing the parent implementations.
   * @return All parent source implementations found.
   */
  def collectInheritedParents(
      source: ArraySeq[SourceLocation.CodeStrict],
      allSource: ArraySeq[SourceLocation.CodeStrict]): ArraySeq[SourceLocation.CodeStrict] =
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
   * @param source    The source trees to search for parent implementations.
   * @param allSource The source trees containing the parent implementations.
   * @return All parent source implementations found.
   */
  def collectInheritedParentsSoft(
      source: ArraySeq[SourceLocation.CodeSoft],
      allSource: ArraySeq[SourceLocation.CodeSoft]): ArraySeq[SourceLocation.CodeSoft] =
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
   * @param allSource The source trees containing the parent implementations.
   * @return All parent source implementations found.
   */
  def collectInheritedParents(
      source: SourceLocation.CodeStrict,
      allSource: ArraySeq[SourceLocation.CodeStrict]): ArraySeq[SourceLocation.CodeStrict] =
    source.tree.ast match {
      case contract: Ast.ContractWithState =>
        collectInheritedParents(
          inheritances = contract.inheritances,
          allSource = allSource,
          processedTrees = mutable.Set(source)
        )

      case _: Ast.Struct | _: Ast.EnumDef[_] | _: Ast.ConstantVarDef[_] | _: Ast.AssetScript =>
        ArraySeq.empty
    }

  /**
   * Collects all parent source implementations inherited by the given
   * source tree within the provided source code files.
   *
   * @param source    The source tree to search for parent implementations.
   * @param allSource The source trees containing the parent implementations.
   * @return All parent source implementations found.
   */
  def collectInheritedParents(
      source: SourceLocation.CodeSoft,
      allSource: ArraySeq[SourceLocation.CodeSoft]): ArraySeq[SourceLocation.CodeSoft] =
    source.part match {
      case contract: SoftAST.Template =>
        collectInheritedParentsSoft(
          inheritances = contract.inheritance,
          allSource = allSource,
          processedTrees = mutable.Set(source)
        )

      case _ =>
        ArraySeq.empty
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
      source: SourceLocation.CodeStrict,
      allSource: ArraySeq[SourceLocation.CodeStrict]): ArraySeq[SourceLocation.CodeStrict] =
    source.tree.ast match {
      case contract: Ast.ContractWithState =>
        collectImplementingChildren(
          contract = contract,
          allSource = allSource,
          processedTrees = mutable.Set(source)
        )

      case _: Ast.Struct | _: Ast.EnumDef[_] | _: Ast.ConstantVarDef[_] | _: Ast.AssetScript =>
        ArraySeq.empty
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
      source: SourceLocation.CodeSoft,
      allSource: ArraySeq[SourceLocation.CodeSoft]): ArraySeq[SourceLocation.CodeSoft] =
    source.part match {
      case contract: SoftAST.Template =>
        collectImplementingChildren(
          template = contract,
          allSource = allSource,
          processedTrees = mutable.Set(source)
        )

      case _ =>
        ArraySeq.empty
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
      allSource: ArraySeq[SourceLocation.CodeStrict],
      processedTrees: mutable.Set[SourceLocation.CodeStrict]): ArraySeq[SourceLocation.CodeStrict] =
    allSource flatMap {
      source =>
        // collect the trees that belong to one of the inheritances
        val belongsToParent =
          source
            .tree
            .typeId()
            .exists {
              typeId =>
                inheritances.exists(_.parentId == typeId)
            }

        // collect the trees that belong to one of the inheritances and the ones that are not already processed
        if (belongsToParent && !processedTrees.contains(source)) {
          processedTrees addOne source

          source.tree.ast match {
            case contract: Ast.ContractWithState =>
              // TODO: There might a need for this to be tail-recursive to avoid stackoverflow on very large codebases.
              val parents =
                collectInheritedParents(
                  inheritances = contract.inheritances,
                  allSource = allSource,
                  processedTrees = processedTrees
                )

              parents :+ source

            case _: Ast.Struct | _: Ast.EnumDef[_] | _: Ast.ConstantVarDef[_] | _: Ast.AssetScript =>
              ArraySeq.empty
          }

        } else {
          ArraySeq.empty
        }
    }

  /**
   * Note: This function is [[SoftAST]] version of the above `StrictAST` [[collectInheritedParents]] implementation.
   *
   * TODO: Being a copy, this is more error-tolerant than "StrictAST", but less error-tolerant than it could be.
   *       To collect implementing children, searching for [[SoftAST.Inheritance]] would yield better results due
   *       to increased error tolerance. But this improvement will be implemented later after the switch to [[SoftAST]]
   *       is complete.
   *
   * Collects all source-trees representing implementations of the provided inheritances.
   *
   * @param inheritances   The inheritances to search for.
   * @param allSource      The source code files containing the inheritance implementations.
   * @param processedTrees A buffer to store processed source trees to avoid duplicate processing.
   *                       This is a mutable collection, so this function must be private.
   * @return All inheritance implementations along with their corresponding source files.
   */
  private def collectInheritedParentsSoft(
      inheritances: Seq[SoftAST.Inheritance],
      allSource: ArraySeq[SourceLocation.CodeSoft],
      processedTrees: mutable.Set[SourceLocation.CodeSoft]): ArraySeq[SourceLocation.CodeSoft] = {
    // collect all names of all inheritance
    val inheritanceNames =
      inheritances.flatMap(_.references.flatMap(_.identifier.toOption.map(_.code.text)))

    allSource flatMap {
      source =>
        // collect the trees if their name that belong to at least one of the inheritances
        val belongsToParent =
          source.part match {
            case template: SoftAST.Template =>
              // check if the template's name is contained in the inheritance list.
              template
                .identifier
                .toOption
                .map(_.code.text) exists inheritanceNames.contains

            case _ =>
              false
          }

        // collect the trees that belong to one of the inheritances and the ones that are not already processed
        if (belongsToParent && !processedTrees.contains(source)) {
          processedTrees addOne source

          source.part match {
            case contract: SoftAST.Template =>
              // TODO: There might a need for this to be tail-recursive to avoid stackoverflow on very large codebases.
              val parents =
                collectInheritedParentsSoft(
                  inheritances = contract.inheritance,
                  allSource = allSource,
                  processedTrees = processedTrees
                )

              parents :+ source

            case _ =>
              ArraySeq.empty
          }

        } else {
          ArraySeq.empty
        }
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
      allSource: ArraySeq[SourceLocation.CodeStrict],
      processedTrees: mutable.Set[SourceLocation.CodeStrict]): ArraySeq[SourceLocation.CodeStrict] =
    allSource flatMap {
      source =>
        val belongs =
          source.tree.ast match {
            case state: Ast.ContractWithState =>
              state.inheritances.exists(_.parentId == contract.ident)

            case _: Ast.Struct | Ast.ConstantVarDef(_, _) | Ast.EnumDef(_, _) | _: Ast.AssetScript =>
              false
          }

        // collect the trees that belong to one of the inheritances and the ones that are not already processed
        if (belongs && !processedTrees.contains(source)) {
          processedTrees addOne source

          source.tree.ast match {
            case contract: Ast.ContractWithState =>
              // TODO: There might a need for this to be tail-recursive to avoid stackoverflow on very large codebases.
              val children =
                collectImplementingChildren(
                  contract = contract,
                  allSource = allSource,
                  processedTrees = processedTrees
                )

              children :+ source

            case _: Ast.Struct | _: Ast.EnumDef[_] | _: Ast.ConstantVarDef[_] | _: Ast.AssetScript =>
              ArraySeq.empty
          }
        } else {
          ArraySeq.empty
        }
    }

  /**
   * Note: This function is [[SoftAST]] version of the above `StrictAST` [[collectImplementingChildren]] implementation.
   *
   * TODO: Being a copy, this is more error-tolerant than "StrictAST", but less error-tolerant than it could be.
   *       To collect implementing children, searching for [[SoftAST.Inheritance]] would yield better results due
   *       to increased error tolerance. But this improvement will be implemented later after the switch to [[SoftAST]]
   *       is complete.
   *
   * Collects all source-trees representing children that implement or extend the given contract.
   *
   * @param template       The contract for which its children are being searched.
   * @param allSource      The source code files containing the inheritances.
   * @param processedTrees A buffer to store processed source trees to avoid duplicate processing.
   *                       This is a mutable collection, so this function must be private.
   * @return All child trees along with their corresponding source files.
   */
  private def collectImplementingChildren(
      template: SoftAST.Template,
      allSource: ArraySeq[SourceLocation.CodeSoft],
      processedTrees: mutable.Set[SourceLocation.CodeSoft]): ArraySeq[SourceLocation.CodeSoft] =
    allSource flatMap {
      source =>
        val belongs =
          collectInheritanceDeclaration(
            inheritanceId = template.identifier,
            target = source.part
          ).nonEmpty

        // collect the trees that belong to one of the inheritances and the ones that are not already processed
        if (belongs && !processedTrees.contains(source)) {
          processedTrees addOne source

          source.part match {
            case contract: SoftAST.Template =>
              // TODO: There might a need for this to be tail-recursive to avoid stackoverflow on very large codebases.
              val children =
                collectImplementingChildren(
                  template = contract,
                  allSource = allSource,
                  processedTrees = processedTrees
                )

              children :+ source

            case _ =>
              ArraySeq.empty
          }
        } else {
          ArraySeq.empty
        }
    }

  /**
   * Collects locations of inheritance declarations defined in the target
   * for the given `inheritanceId`.
   *
   * @param inheritanceId The identifier of the inheritance to search for within the target.
   * @param target        The target being searched.
   * @return An iterator over the locations where these inheritances are defined.
   */
  private def collectInheritanceDeclaration(
      inheritanceId: SoftAST.IdentifierAST,
      target: SoftAST.BlockPartAST): Iterator[SoftAST.ReferenceCallOrIdentifier] =
    inheritanceId match {
      case inheritanceId: SoftAST.Identifier =>
        collectInheritanceDeclaration(
          inheritanceId = inheritanceId,
          target = target
        )

      case SoftAST.IdentifierExpected(_) =>
        Iterator.empty
    }

  /**
   * Collects locations of inheritance declarations defined in the target
   * for the given `inheritanceId`.
   *
   * @param inheritanceId The identifier of the inheritance to search for within the target.
   * @param target        The target being searched.
   * @return An iterator over the locations where these inheritances are defined.
   */
  private def collectInheritanceDeclaration(
      inheritanceId: SoftAST.Identifier,
      target: SoftAST.BlockPartAST): Iterator[SoftAST.ReferenceCallOrIdentifier] =
    target match {
      case template: SoftAST.Template =>
        collectInheritanceDeclaration(
          inheritanceId = inheritanceId,
          target = template
        )

      case _ =>
        Iterator.empty
    }

  /**
   * Collects locations of inheritance declarations defined in the target
   * for the given `inheritanceId`.
   *
   * @param inheritanceId The identifier of the inheritance to search for within the target.
   * @param target        The target being searched.
   * @return An iterator over the locations where these inheritances are defined.
   */
  private def collectInheritanceDeclaration(
      inheritanceId: SoftAST.Identifier,
      target: SoftAST.Template): Iterator[SoftAST.ReferenceCallOrIdentifier] =
    target.inheritance.iterator.flatMap(_.references).collect {
      case thisId: SoftAST.Identifier if thisId.code.text == inheritanceId.code.text =>
        thisId

      case refCall @ SoftAST.ReferenceCall(_, SoftAST.Identifier(_, _, code), _, _) if code.text == inheritanceId.code.text =>
        refCall
    }

}
