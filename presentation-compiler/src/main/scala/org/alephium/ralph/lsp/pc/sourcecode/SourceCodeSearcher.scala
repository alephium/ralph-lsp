package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.sourcecode.error._

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer

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
            Left(SourceCodeHasCompilationErrors(fileURI))

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
   * Collects all parent source implementations inherited by the given
   * source tree within the provided source code files.
   *
   * @param source    The source tree to search for parent implementations.
   * @param allSource The source code files containing the parent implementations.
   * @return All parent source implementations found.
   */
  def collectInheritanceInScope(
      source: Tree.Source,
      allSource: ArraySeq[SourceCodeState.Parsed]): Seq[SourceTreeInScope] =
    source.ast match {
      case Left(contract) =>
        collectParentsInherited(
          inheritances = contract.inheritances,
          allSource = allSource,
          processedTrees = ListBuffer(source)
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
   *                       This is a mutable collection so this function must be private.
   * @return All inheritance implementations along with their corresponding source files.
   */
  private def collectParentsInherited(
      inheritances: Seq[Ast.Inheritance],
      allSource: ArraySeq[SourceCodeState.Parsed],
      processedTrees: ListBuffer[Tree.Source]): Seq[SourceTreeInScope] =
    if (inheritances.isEmpty) // Early check: Do not traverse workspace source-code if inheritances are empty.
      Seq.empty
    else
      allSource flatMap {
        parsed =>
          parsed.ast.statements flatMap {
            // collect the trees that belong to one of the inheritances and the ones that are not already processed
            case source: Tree.Source if inheritances.exists(_.parentId == source.typeId()) && !processedTrees.contains(source) =>
              processedTrees addOne source

              source.ast match {
                case Left(contract) =>
                  // TODO: There might a need for this to be tail-recursive to avoid stackoverflow on very large codebases.
                  val parents =
                    collectParentsInherited(
                      inheritances = contract.inheritances,
                      allSource = allSource,
                      processedTrees = processedTrees
                    )

                  parents :+ SourceTreeInScope(source, parsed)

                case Right(_) =>
                  Seq(SourceTreeInScope(source, parsed))
              }

            case _ =>
              Seq.empty
          }
      }

}
