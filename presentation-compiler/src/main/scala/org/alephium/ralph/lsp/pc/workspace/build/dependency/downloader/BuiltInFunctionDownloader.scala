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

package org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.{SourceIndex, BuiltIn}

import java.nio.file.Path
import scala.collection.immutable.ArraySeq

object BuiltInFunctionDownloader extends DependencyDownloader.Native {

  override def dependencyID: DependencyID.BuiltIn.type =
    DependencyID.BuiltIn

  /**
   * Downloads built-in function source files.
   *
   * @param dependencyPath The directory where dependencies are located.
   * @return An iterable over the downloaded built-in source files.
   */
  protected def _download(
      dependencyPath: Path,
      errorIndex: SourceIndex
    )(implicit logger: ClientLogger): Either[ArraySeq[CompilerMessage.AnyError], WorkspaceState.UnCompiled] = {
    val workspaceDir =
      dependencyPath resolve dependencyID.dirName

    val sourceCode =
      toSourceCodeState(
        workspacePath = workspaceDir,
        functions = BuiltInFunctionInfo.build()
      )

    // a default build file.
    val build =
      DependencyDownloader.defaultBuild(workspaceDir)

    val workspace =
      WorkspaceState.UnCompiled(
        build = build,
        sourceCode = sourceCode.to(ArraySeq)
      )

    Right(workspace)
  }

  /**
   * Converts [[BuiltInFunctionInfo]] to [[SourceCodeState.UnCompiled]].
   *
   * @param workspacePath The directory where dependencies are located.
   * @param functions     The built-in function information.
   * @return An iterable over the converted [[SourceCodeState.UnCompiled]] objects.
   */
  private def toSourceCodeState(
      workspacePath: Path,
      functions: Seq[BuiltInFunctionInfo]): Iterable[SourceCodeState.UnCompiled] =
    functions
      .groupBy(_.category)
      .map {
        case (category, functions) =>
          val interface =
            toInterface(
              category = category,
              functions = functions
            )

          val filePath =
            workspacePath.resolve(s"${category.toString().toLowerCase}_functions.${CompilerAccess.RALPH_FILE_EXTENSION}")

          SourceCodeState.UnCompiled(
            fileURI = filePath.toUri,
            code = interface
          )
      }

  /**
   * Converts built-in function information to a Ralph Interface which is parseable and compilable.
   *
   * @param category  The category of built-in functions.
   * @param functions The built-in function information.
   * @return A string representing the generated interface.
   */
  private def toInterface(
      category: BuiltIn.Category,
      functions: Seq[BuiltInFunctionInfo]): String = {
    val functionsCode =
      functions
        .sorted
        .map {
          function =>
            val params =
              if (function.params.nonEmpty)
                function
                  .params
                  .map {
                    param =>
                      s"  // $param"
                  }
                  .mkString("\n", "\n", "")
              else
                ""

            s"""  // ${function.doc}$params
               |  // ${function.returns}
               |  ${function.signature}
               |""".stripMargin
        }
        .mkString("\n")

    val interface =
      s"""Interface ${category}Functions {
         |
         |$functionsCode
         |}
         |""".stripMargin

    // replace the non-compilable code with compilable code.
    replaceNonStandardCode(interface)
  }

  /**
   * Replaces non-standard code elements.
   *
   * @param code The code string to be processed.
   * @return The code string with non-standard elements replaced.
   */
  private def replaceNonStandardCode(code: String): String =
    code
      .replaceAll("\\?:", ":")
      .replaceAll("<Contract>", "TheContract")
      .replaceAll("Bool\\|I256\\|U256\\|Address", "From")
      .replaceAll("\\.\\.\\.any", "any: Sequence")
      .replaceAll("fn len!\\(array\\)", "fn len!(array: Array)")

}
