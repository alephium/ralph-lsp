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

package org.alephium.ralph.lsp.pc.search

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.access.util.TestCodeUtil
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.search.completion.Suggestion
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, TestSourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.build.TestBuild
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, TestWorkspace, Workspace}
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers._

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

object TestCodeProvider {

  /**
   * Runs completion where `@@` is.
   *
   * For example: The following runs completion between the double quotes.
   * {{{
   *   TestCompleter(""" import "@@" """)
   * }}}
   */
  private def apply[A](code: String)(implicit provider: CodeProvider[A]): (Iterator[A], SourceCodeState.IsCodeAware, WorkspaceState.IsParsedAndCompiled) = {
    val (linePosition, _, codeWithoutAtSymbol) = TestCodeUtil.indicatorPosition(code)

    // run completion at that line and character
    val (searchResult, workspace) =
      TestCodeProvider(
        line = linePosition.line,
        character = linePosition.character,
        code = codeWithoutAtSymbol
      )

    workspace.sourceCode should have size 1

    // delete the workspace
    TestWorkspace delete workspace

    (searchResult.value, workspace.sourceCode.head.asInstanceOf[SourceCodeState.IsCodeAware], workspace)

  }

  /**
   * Runs GoTo definition where `@@` is located
   * and expects the go-to location to be the text
   * between the symbols `>>...<<`.
   *
   * If the go-to symbols are not provided, then it expects empty result.
   *
   * @param code The containing `@@` and `>>...<<` symbols.
   */
  def goTo(code: String): Assertion = {
    val (expectedLineRanges, codeWithoutGoToSymbols, _, _) =
      TestCodeUtil.lineRanges(code)

    // Execute go-to definition.
    val (searchResult, sourceCode, _) =
      TestCodeProvider[SourceLocation.GoTo](codeWithoutGoToSymbols)

    // Expect GoToLocations to also contain the fileURI
    val expectedGoToLocations =
      expectedLineRanges map {
        lineRange =>
          (sourceCode.fileURI, lineRange)
      }

    // For actual search result assert only the fileURI and line-ranges
    val actual =
      searchResult
        .toList
        .map {
          result =>
            (result.parsed.fileURI, result.toLineRange().value)
        }

    // assert that the go-to definition jumps to all text between the go-to symbols << and >>
    actual should contain theSameElementsAs expectedGoToLocations
  }

  /**
   * Runs go-to definition where `@@` is positioned, expecting
   * the resulting go-to definition to be a built-in function.
   *
   * @param code     The code with the search indicator '@@'.
   * @param expected Expected resulting built-in function.
   */
  def goToBuiltIn(
      code: String,
      expected: Option[String]): Assertion = {
    val (_, codeWithoutGoToSymbols, _, _) =
      TestCodeUtil.lineRanges(code)

    // Execute go-to definition.
    val (searchResult, _, workspace) =
      TestCodeProvider[SourceLocation.GoTo](codeWithoutGoToSymbols)

    expected match {
      case Some(expectedFunction) =>
        val expectedResults =
          workspace
            .build
            .findDependency(DependencyID.BuiltIn)
            .to(ArraySeq)
            .flatMap(_.sourceCode)
            .filter(_.code.contains(expectedFunction)) // filter built-in workspace's source-files that contain this built-in function.
            .flatMap {
              builtInFile =>
                // insert symbol >>..<<
                val codeWithRangeSymbols =
                  builtInFile
                    .code
                    .replace(expectedFunction, s">>$expectedFunction<<")

                // compute the line range
                TestCodeUtil
                  .lineRanges(codeWithRangeSymbols)
                  ._1
                  .map {
                    lineRange =>
                      (builtInFile.fileURI, lineRange)
                  }
            }

        // For actual search result assert only the fileURI and line-ranges
        val actualResults =
          searchResult
            .toList
            .map {
              result =>
                (result.parsed.fileURI, result.toLineRange().value)
            }

        // assert that the go-to definition jumps to all text between the go-to symbols << and >>
        actualResults should contain theSameElementsAs expectedResults

      case None =>
        searchResult shouldBe empty
    }
  }

  /**
   * Runs code completion where `@@` is positioned.
   *
   * @param code The code to run code completion on.
   * @return A list of code completion suggestions.
   */
  def suggest(code: String): List[Suggestion] =
    TestCodeProvider[Suggestion](code)
      ._1
      .toList

  /**
   * Run test completion.
   *
   * @param line      The target line number
   * @param character The target character within the line
   * @param code      The code to run completion on.
   * @return Suggestions and the created workspace.
   */
  private def apply[A](
      line: Int,
      character: Int,
      code: Gen[String]
    )(implicit provider: CodeProvider[A]): (Either[CompilerMessage.Error, Iterator[A]], WorkspaceState.IsParsedAndCompiled) = {
    implicit val clientLogger: ClientLogger = TestClientLogger
    implicit val file: FileAccess           = FileAccess.disk
    implicit val compiler: CompilerAccess   = CompilerAccess.ralphc

    // create a build file
    val build =
      TestBuild
        .genCompiledOK()
        .sample
        .get

    // Generate a source-file name within the contract URI
    val sourceFile =
      TestFile
        .genFileURI(rootFolder = Paths.get(build.contractURI))
        .sample
        .get

    // write the source code
    val (sourceCode, _) =
      TestSourceCode
        .genOnDiskAndPersist(
          fileURI = sourceFile,
          code = code.sample.get
        )
        .sample
        .get

    // create a workspace for the build file
    val workspace =
      WorkspaceState.UnCompiled(
        build = build,
        sourceCode = ArraySeq(sourceCode)
      )

    // parse and compile workspace
    val compiledWorkspace =
      Workspace.parseAndCompile(workspace)

    // execute completion.
    val completionResult =
      CodeProvider.search(
        line = line,
        character = character,
        fileURI = sourceCode.fileURI,
        workspace = compiledWorkspace
      )

    (completionResult.value, compiledWorkspace)
  }

}
