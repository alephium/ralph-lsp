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

import org.alephium.ralph.lsp.TestCommon
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.access.util.TestCodeUtil
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.search.completion.Suggestion
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, TestSourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.build.{TestRalphc, BuildState, TestBuild}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.{DependencyID, TestDependency}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader.{StdInterfaceDownloader, DependencyDownloader, BuiltInFunctionDownloader}
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, TestWorkspace, Workspace}
import org.scalatest.Assertion
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers._

import java.net.URI
import scala.collection.immutable.ArraySeq

object TestCodeProvider {

  /**
   * Runs code completion where `@@` is positioned.
   *
   * @param code The code to run code completion on.
   * @return A list of code completion suggestions.
   */
  def suggest(code: String): List[Suggestion] =
    TestCodeProvider[Unit, Suggestion](
      code = code,
      searchSettings = (),
      dependencyDownloaders = DependencyDownloader.natives()
    )._1.toList

  /**
   * Runs GoTo definition where `@@` is located
   * and expects the go-to location to be the text
   * between the symbols `>>...<<`.
   *
   * If the go-to symbols are not provided, then it expects an empty result.
   *
   * @param code The containing `@@` and `>>...<<` symbols.
   */
  def goToDefinition(code: String): Assertion =
    goTo[Unit, SourceLocation.GoToDef](
      code = code,
      searchSettings = ()
    )

  def goToReferences(code: String): Assertion =
    goTo[Boolean, SourceLocation.GoToRef](
      code = code,
      searchSettings = false
    )

  /**
   * Runs GoTo definition where `@@` is located
   * and expects the go-to location to be the text
   * between the symbols `>>...<<`.
   *
   * If the go-to symbols are not provided, then it expects an empty result.
   *
   * @param code The containing `@@` and `>>...<<` symbols.
   */
  def goTo[I, O <: SourceLocation.GoTo](
      code: String,
      searchSettings: I
    )(implicit codeProvider: CodeProvider[I, O]): Assertion = {
    val (expectedLineRanges, codeWithoutGoToSymbols, _, _) =
      TestCodeUtil.lineRanges(code)

    // Execute go-to definition.
    val (searchResult, sourceCode, _) =
      TestCodeProvider[I, O](
        code = codeWithoutGoToSymbols,
        searchSettings = searchSettings,
        dependencyDownloaders = ArraySeq.empty
      )

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
   * Tests directly on the `builtin` native library.
   *
   * Runs go-to definition where `@@` is positioned, expecting
   * the resulting go-to definition to be a built-in function
   * contained in the `builtin` library downloaded by native dependency
   * downloader [[BuiltInFunctionDownloader]].
   *
   * @param code     The code with the search indicator '@@'.
   * @param expected Expected resulting built-in function.
   */
  def goToBuiltIn(
      code: String,
      expected: Option[String]): Assertion =
    goToDependency(
      code = code,
      expected = expected.map {
        string =>
          (string, string)
      },
      downloader = BuiltInFunctionDownloader
    )

  /**
   * Tests directly on the `std` native library.
   *
   * Runs go-to definition where `@@` is positioned, expecting
   * the resulting go-to definition to be in a std file
   * contained in the `std` library downloaded by native dependency
   * downloader [[StdInterfaceDownloader]].
   *
   * @param code     The code with the search indicator '@@'.
   * @param expected An optional tuple where the first element is the expected line,
   *                 and the second is the highlighted token in that line.
   */
  def goToStd(
      code: String,
      expected: Option[(String, String)]): Assertion =
    goToDependency(
      code = code,
      expected = expected,
      downloader = StdInterfaceDownloader
    )

  /**
   * Runs go-to definition on a custom dependency and workspace source-code.
   * Other go-to functions test directly on native `std` and `builtin` libraries,
   * but this allows creating custom dependency code.
   *
   * @param dependencyId The dependency ID to assign to the custom dependency.
   * @param dependency   The dependency code to write to the dependency.
   * @param workspace    The developer's workspace code.
   * @return
   */
  def goToReferences(
      dependencyId: DependencyID,
      dependency: String,
      workspace: String): Unit =
    goTo[Boolean, SourceLocation.GoToRef](
      dependencyId = dependencyId,
      dependency = dependency,
      workspace = workspace,
      searchSettings = false
    )

  def goToDefinition(
      dependencyId: DependencyID,
      dependency: String,
      workspace: String): Unit =
    goTo[Unit, SourceLocation.GoToDef](
      dependencyId = dependencyId,
      dependency = dependency,
      workspace = workspace,
      searchSettings = ()
    )

  /**
   * Runs go-to definition on a custom dependency and workspace source-code.
   * Other go-to functions test directly on native `std` and `builtin` libraries,
   * but this allows creating custom dependency code.
   *
   * @param dependencyId The dependency ID to assign to the custom dependency.
   * @param dependency   The dependency code to write to the dependency.
   * @param workspace    The developer's workspace code.
   * @return
   */
  private def goTo[I, O <: SourceLocation.GoTo](
      dependencyId: DependencyID,
      dependency: String,
      workspace: String,
      searchSettings: I
    )(implicit codeProvider: CodeProvider[I, O]): Unit = {
    implicit val clientLogger: ClientLogger = TestClientLogger
    implicit val file: FileAccess           = FileAccess.disk
    implicit val compiler: CompilerAccess   = CompilerAccess.ralphc

    // The indicator's gotta be in either the dependency or the workspace code.
    val isIndicatorInDependency =
      dependency contains TestCodeUtil.SEARCH_INDICATOR

    val (indicatorPosition, _, codeWithoutIndicatorMarker) =
      if (isIndicatorInDependency)
        TestCodeUtil.indicatorPosition(dependency) // maybe the indicator in dependency code
      else
        TestCodeUtil.indicatorPosition(workspace) // otherwise, the indicator must be in dependency code

    val (dependencyLineRange, dependencyCodeWithoutRangeMarkers, _, _) =
      if (isIndicatorInDependency)
        TestCodeUtil.lineRanges(codeWithoutIndicatorMarker) // indicator existed in dependency code, use codeWithoutIndicatorMarker
      else
        TestCodeUtil.lineRanges(dependency) // no indicator in dependency code, use dependency directly

    val (workspaceCodeRange, workspaceCodeWithoutRangeMarkers, _, _) =
      if (isIndicatorInDependency)
        TestCodeUtil.lineRanges(workspace) // no indicator in workspace code, use workspace directly
      else
        TestCodeUtil.lineRanges(codeWithoutIndicatorMarker) // indicator existed in workspace code, use codeWithoutIndicatorMarker

    // collect all line ranges, they could be in both dependency and workspace code
    val expectedRanges =
      dependencyLineRange ++ workspaceCodeRange

    // build a custom dependency from dependency code
    val downloader =
      TestDependency.buildDependencyDownloader(
        depId = dependencyId,
        depCode = dependencyCodeWithoutRangeMarkers
      )

    // create a build file
    val build =
      TestCommon
        .genName
        .flatMap {
          dependenciesFolderName =>
            TestBuild
              .genCompiledOK(
                // since custom dependencies are being written; always set a dependency folder name,
                // so dependency files get generated local to the workspace,
                // otherwise they will get written to `~/.ralph-lsp/*`.
                config = TestRalphc.genRalphcParsedConfig(dependenciesFolderName = Some(dependenciesFolderName)),
                dependencyDownloaders = ArraySeq(downloader)
              )
        }
        .sample
        .get

    // the one dependency is written with custom code.
    build.dependencies should have size 1
    build.dependencies.head.sourceCode should have size 1
    val dependencySourceFile = build.dependencies.head.sourceCode.head
    dependencySourceFile.code shouldBe dependencyCodeWithoutRangeMarkers

    // generate an on-disk workspace source-file.
    val sourceCode =
      TestSourceCode
        .genOnDiskAndPersist(
          build = build,
          code = workspaceCodeWithoutRangeMarkers
        )
        .sample
        .get

    // use the selected fileURI to be the one with @@ indicator.
    val selectedFileURI =
      if (isIndicatorInDependency)
        dependencySourceFile.fileURI
      else
        sourceCode.fileURI

    // run test
    val (searchResult, testWorkspace) =
      TestCodeProvider[I, O](
        line = indicatorPosition.line,
        character = indicatorPosition.character,
        selectedFileURI = selectedFileURI,
        searchSettings = searchSettings,
        build = build,
        workspaceSourceCode = sourceCode
      )

    testWorkspace.sourceCode should have size 1
    testWorkspace.build.dependencies should have size 1

    val actualLineRanges = searchResult.value.flatMap(_.toLineRange()).toList
    actualLineRanges should contain theSameElementsAs expectedRanges

    TestWorkspace delete testWorkspace
    ()
  }

  /**
   * Runs go-to definition where @@ is positioned, expecting
   * the resulting go-to definition to be within a dependency workspace.
   *
   * @param code       The code with the search indicator '@@'.
   * @param expected   An optional tuple where the first element is the expected line,
   *                   and the second is the highlighted token in that line.
   * @param downloader The native dependency to download, and to test on.
   *                   These must be of type [[DependencyDownloader.Native]]
   *                   as they can be written to `~/ralph-lsp`.
   *                   We don't want generated libraries being written to `~/ralph-lsp`.
   */
  private def goToDependency(
      code: String,
      expected: Option[(String, String)],
      downloader: DependencyDownloader.Native): Assertion = {
    val (_, codeWithoutGoToSymbols, _, _) =
      TestCodeUtil.lineRanges(code)

    // Execute go-to definition.
    val (searchResult, _, workspace) =
      TestCodeProvider[Unit, SourceLocation.GoToDef](
        code = codeWithoutGoToSymbols,
        searchSettings = (),
        dependencyDownloaders = ArraySeq(downloader)
      )

    expected match {
      case Some((expectedLine, expectedHighlightedToken)) =>
        val expectedResults =
          workspace
            .build
            .findDependency(downloader.dependencyID)
            .to(ArraySeq)
            .flatMap(_.sourceCode)
            .filter(_.code.contains(expectedLine)) // filter source-files that contain this code function.
            .flatMap {
              builtInFile =>
                // insert symbol >>..<<
                val codeWithRangeSymbols =
                  builtInFile
                    .code
                    .replace(expectedHighlightedToken, s">>$expectedHighlightedToken<<")

                if (builtInFile.code == codeWithRangeSymbols)
                  fail(s"Could not find the expected highlighted token '$expectedHighlightedToken' in line '$expectedLine'.")

                // compute the line range
                TestCodeUtil
                  .lineRanges(codeWithRangeSymbols)
                  ._1
                  .map {
                    lineRange =>
                      (builtInFile.fileURI, lineRange)
                  }
            }

        if (expectedResults.isEmpty)
          fail(s"Could not find the expected line '$expectedLine'.")

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
   * Runs code-provider on the given code that contains the selection indicator '@@'.
   *
   * @param code                  The code with the search indicator '@@'.
   * @param dependencyDownloaders The native dependency to download, and to test on.
   *                              These must be of type [[DependencyDownloader.Native]]
   *                              as they can be written to `~/ralph-lsp`.
   *                              We don't want generated libraries being written to `~/ralph-lsp`.
   */
  private def apply[I, O](
      code: String,
      searchSettings: I,
      dependencyDownloaders: ArraySeq[DependencyDownloader.Native]
    )(implicit provider: CodeProvider[I, O]): (Iterator[O], SourceCodeState.IsCodeAware, WorkspaceState.IsParsedAndCompiled) = {
    implicit val clientLogger: ClientLogger = TestClientLogger
    implicit val file: FileAccess           = FileAccess.disk
    implicit val compiler: CompilerAccess   = CompilerAccess.ralphc

    val (linePosition, _, codeWithoutAtSymbol) =
      TestCodeUtil.indicatorPosition(code)

    // create a build file
    val build =
      TestBuild
        .genCompiledOK(dependencyDownloaders = dependencyDownloaders)
        .sample
        .get

    // generate source-code for the code
    val sourceCode =
      TestSourceCode
        .genOnDiskAndPersist(
          build = build,
          code = codeWithoutAtSymbol
        )
        .sample
        .get

    // run completion at that line and character
    val (searchResult, workspace) =
      TestCodeProvider(
        line = linePosition.line,
        character = linePosition.character,
        selectedFileURI = sourceCode.fileURI,
        searchSettings = searchSettings,
        build = build,
        workspaceSourceCode = sourceCode
      )

    workspace.sourceCode should have size 1

    // delete the workspace
    TestWorkspace delete workspace

    (searchResult.value, workspace.sourceCode.head.asInstanceOf[SourceCodeState.IsCodeAware], workspace)

  }

  /**
   * Runs test on the given [[CodeProvider]], at the given line and character number.
   *
   * @param line                The target line number
   * @param character           The target character within the line
   * @param selectedFileURI     The URI of the source-file where this search is to be executed.
   * @param workspaceSourceCode The source to write to the test workspace.
   * @return Suggestions and the created workspace.
   */
  private def apply[I, O](
      line: Int,
      character: Int,
      selectedFileURI: URI,
      searchSettings: I,
      build: BuildState.Compiled,
      workspaceSourceCode: SourceCodeState.OnDisk
    )(implicit provider: CodeProvider[I, O],
      client: ClientLogger,
      file: FileAccess,
      compiler: CompilerAccess): (Either[CompilerMessage.Error, Iterator[O]], WorkspaceState.IsParsedAndCompiled) = {

    // create a workspace for the build file
    val workspace =
      WorkspaceState.UnCompiled(
        build = build,
        sourceCode = ArraySeq(workspaceSourceCode)
      )

    // parse and compile workspace
    val compiledWorkspace =
      Workspace.parseAndCompile(workspace)

    // execute code-provider.
    val completionResult =
      CodeProvider.search(
        line = line,
        character = character,
        fileURI = selectedFileURI,
        workspace = compiledWorkspace,
        searchSettings = searchSettings
      )

    (completionResult.value, compiledWorkspace)
  }

}
