package org.alephium.ralph.lsp.pc.sourcecode.imports

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.error.ImportError
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState

import scala.collection.immutable.ArraySeq

object Importer {

  /**
   * Perform type-check on import statements in all source-files within the workspace.
   *
   * @param sourceCode Workspace source code files that might contain import statements.
   * @param dependency Dependent code is searched to find imported code.
   * @return - Left: Errors, if there were type errors
   *         - Right: The imported code.
   */
  def typeCheck(sourceCode: ArraySeq[SourceCodeState.Parsed],
                dependency: Option[ArraySeq[SourceCodeState.Compiled]]): Either[ArraySeq[SourceCodeState.ErrorSource], ArraySeq[SourceCodeState.Compiled]] = {
    val dependencyOrEmpty =
      dependency getOrElse ArraySeq.empty

    // run import type check on every source file
    val imported =
      sourceCode map {
        sourceCode =>
          typeCheck(
            sourceCode = sourceCode,
            dependency = dependencyOrEmpty
          )
      }

    val (importErrors, compiledDep) =
      imported partitionMap identity

    if (importErrors.nonEmpty) {
      // there are import type check errors
      Left(importErrors)
    } else {
      // import type checks passed, return unique imports.
      val distinctDeps =
        compiledDep.flatten.distinctBy(_.importIdentifier)

      Right(distinctDeps)
    }
  }

  /**
   * Perform type-check on all import statements with in a single source file.
   *
   * @param sourceCode Source code of a file that might contain import statements.
   * @param dependency Dependent code is searched to find imported code.
   * @return - Left: Errors, if there were type errors
   *         - Right: The imported code.
   */
  def typeCheck(sourceCode: SourceCodeState.Parsed,
                dependency: ArraySeq[SourceCodeState.Compiled]): Either[SourceCodeState.ErrorSource, Seq[SourceCodeState.Compiled]] = {
    val imported =
      sourceCode.ast.statements collect {
        case imported: Tree.Import => // type all import statements
          // TODO: Build a cached Map stored in BuildState instead of doing this linear search.
          // import statement should exists in dependant code.
          dependency.find(_.importIdentifier.map(_.string.name.value) contains imported.string.name.value) match {
            case Some(dependency) =>
              Right(dependency) // Import exists! Good!

            case None =>
              // Import does not exists. Report unknown import error.
              val errorMessage =
                ImportError.Unknown(imported)

              Left(errorMessage)
          }
      }

    val (importErrors, importedCode) =
      imported partitionMap identity

    if (importErrors.nonEmpty) {
      // build a source-code state that reports all import errors in this file.
      val sourceError =
        SourceCodeState.ErrorSource(
          fileURI = sourceCode.fileURI,
          code = sourceCode.code,
          errors = importErrors,
          previous = Some(sourceCode)
        )

      Left(sourceError)
    } else {
      Right(importedCode)
    }
  }

}
