// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.sourcecode.imports

import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.error.ImportError
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.utils.URIUtil

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters.IteratorHasAsScala

object Importer {

  /**
   * Perform type-check on import statements in all source-files within the workspace.
   *
   * @param sourceCode Workspace source code files that might contain import statements.
   * @param dependency Dependent code is searched to find imported code.
   * @return - Left: Errors, if there were type errors
   *         - Right: The imported code.
   */
  def typeCheck(
      sourceCode: ArraySeq[SourceCodeState.Parsed],
      dependency: ArraySeq[SourceCodeState.Compiled]): Either[ArraySeq[SourceCodeState.ErrorCompilation], ArraySeq[SourceCodeState.Compiled]] = {
    // run import type check on every source file
    val imported =
      sourceCode map {
        sourceCode =>
          typeCheck(
            sourceCode = sourceCode,
            dependency = dependency
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
  def typeCheck(
      sourceCode: SourceCodeState.Parsed,
      dependency: ArraySeq[SourceCodeState.Compiled]): Either[SourceCodeState.ErrorCompilation, Seq[SourceCodeState.Compiled]] = {
    val imported =
      sourceCode.astStrict.statements collect {
        case imported: Tree.Import => // type all import statements
          // TODO: Build a cached Map stored in BuildState instead of doing this linear search.
          // import statement should exists in dependant code.
          dependency.find(_.importIdentifier.exists(_.string.name.value == imported.string.name.value)) match {
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
        SourceCodeState.ErrorCompilation(
          errors = importErrors,
          parsed = sourceCode
        )

      Left(sourceError)
    } else {
      Right(importedCode)
    }
  }

  /**
   * String literal that defines an import statement for a source file.
   *
   * @return If the file is named `std/my_code.ral`, the import statement returned
   *         is `import "std/my_code"`.
   */
  def importIdentifier(uri: URI): Option[String] =
    URIUtil.takeRight(
      uri = uri,
      count = 2
    ) map {
      identifier =>
        val string =
          identifier
            .iterator()
            .asScala
            .mkString("/") // import statements use forward slash.

        val identifierWithVersion =
          string.substring(0, string.lastIndexOf("."))

        // Import statements do not contain version numbers, remove them.
        DependencyID.all().foldLeft(identifierWithVersion) {
          case (ident, dependencyID) =>
            ident.replace(dependencyID.dirName, dependencyID.importName)
        }
    }

}
