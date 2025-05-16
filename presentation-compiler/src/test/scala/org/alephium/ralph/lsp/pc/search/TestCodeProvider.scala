// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search

import org.alephium.ralph.lsp.TestCommon
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.access.util.{StringUtil, TestCodeUtil}
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.search.completion.Suggestion
import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefSetting
import org.alephium.ralph.lsp.pc.search.gotoref.GoToRefSetting
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation, TestSourceCode}
import org.alephium.ralph.lsp.pc.workspace.{TestWorkspace, Workspace, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.{TestBuild, TestRalphc}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.{DependencyID, TestDependency}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader.{BuiltInFunctionDownloader, DependencyDownloader, StdInterfaceDownloader}
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.Assertion

import java.net.URI
import scala.collection.immutable.ArraySeq
import scala.util.matching.Regex

object TestCodeProvider {

  val testGoToDefSetting: GoToDefSetting =
    GoToDefSetting(
      includeAbstractFuncDef = false,
      includeInheritance = true
    )

  val testGoToRefSetting: GoToRefSetting =
    GoToRefSetting(
      includeDeclaration = false,
      includeTemplateArgumentOverrides = false,
      includeEventFieldReferences = true,
      goToDefSetting = testGoToDefSetting
    )

  /**
   * Runs code completion where `@@` is positioned.
   *
   * @param code The code to run code completion on.
   * @return A list of code completion suggestions.
   */
  def suggest(code: String*): List[Suggestion] =
    TestCodeProvider[SourceCodeState.Parsed, Unit, Suggestion](
      code = code.to(ArraySeq),
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
  def goToDefinitionStrict(settings: GoToDefSetting = testGoToDefSetting)(code: String*): List[SourceLocation.GoToDefStrict] =
    goTo[SourceCodeState.Parsed, GoToDefSetting, SourceLocation.GoToDefStrict](
      code = code.to(ArraySeq),
      searchSettings = settings,
      dependencyDownloaders = ArraySeq.empty
    )

  def goToDefinitionSoft(settings: GoToDefSetting = testGoToDefSetting)(code: String*): List[SourceLocation.GoToDefSoft] =
    goTo[SourceCodeState.IsParsed, (SoftAST.type, GoToDefSetting), SourceLocation.GoToDefSoft](
      code = code.to(ArraySeq),
      searchSettings = (SoftAST, settings),
      dependencyDownloaders = ArraySeq.empty
    )

  /** Executes go-to-definition providers for both StrictAST and [[SoftAST]] */
  def goToDefinition(settings: GoToDefSetting = testGoToDefSetting)(code: String*): List[SourceLocation.GoToDefSoft] = {
    val resultStrict       = goToDefinitionStrict(settings)(code: _*)
    val resultStrictRanges = resultStrict.map(_.toLineRange().value)

    val resultSoft       = goToDefinitionSoft(settings)(code: _*)
    val resultSoftRanges = resultSoft.map(_.toLineRange().value)

    // Assert that both go-to-def services (Strict & Soft) return the same result.
    resultSoftRanges should contain theSameElementsAs resultStrictRanges
    // return either one of the results because they both contain the same result
    resultSoft
  }

  def goToDefinitionForAll(
      referencesFinder: Regex,
      referenceReplacement: String,
      settings: GoToDefSetting = testGoToDefSetting
    )(code: String): Unit =
    goToForAll[SourceCodeState.Parsed, GoToDefSetting, SourceLocation.GoToDefStrict](
      finder = referencesFinder,
      replacer = referenceReplacement,
      settings = settings,
      code = code
    )

  def goToReferences(settings: GoToRefSetting = testGoToRefSetting)(code: String*): List[SourceLocation.GoToRefStrict] =
    goTo[SourceCodeState.Parsed, GoToRefSetting, SourceLocation.GoToRefStrict](
      code = code.to(ArraySeq),
      searchSettings = settings,
      dependencyDownloaders = ArraySeq.empty
    )

  def goToRename(code: String*): List[SourceLocation.GoToRenameStrict] =
    goTo[SourceCodeState.Parsed, Unit, SourceLocation.GoToRenameStrict](
      code = code.to(ArraySeq),
      searchSettings = (),
      dependencyDownloaders = ArraySeq.empty
    )

  def goToTypeDef(code: String*): List[SourceLocation.GoToTypeDef] =
    goTo[SourceCodeState.Parsed, Unit, SourceLocation.GoToTypeDef](
      code = code.to(ArraySeq),
      searchSettings = (),
      dependencyDownloaders = ArraySeq.empty
    )

  /**
   * Background: Go-to references should output the same result, regardless of whether it is executed
   * on the definition/declaration or on the usages.
   *
   * This function first runs "Find References" on the given code (which is executed on the declaration)
   * and then on all references. It asserts that, in both cases, the references returned must ALWAYS be the same.
   *
   * @param referencesFinder     The regex that matches references, eg: `>>MyContract<<`.
   * @param referenceReplacement The string used to replace references in the code, eg: `>>MyCont@@ract<<`.
   * @param code                 Initial code to run the test on.
   * @return List of ranges where the references were found.
   */
  def goToReferencesForAll(
      referencesFinder: Regex,
      referenceReplacement: String,
      settings: GoToRefSetting = testGoToRefSetting
    )(code: String): Unit =
    goToForAll[SourceCodeState.Parsed, GoToRefSetting, SourceLocation.GoToRefStrict](
      finder = referencesFinder,
      replacer = referenceReplacement,
      settings = settings,
      code = code
    )

  def goToRenameForAll(
      renameFinder: Regex,
      renameReplacer: String
    )(code: String): Unit =
    goToForAll[SourceCodeState.Parsed, Unit, SourceLocation.GoToRenameStrict](
      finder = renameFinder,
      replacer = renameReplacer,
      settings = (),
      code = code
    )

  private def goToForAll[S, I, O <: SourceLocation.GoTo](
      finder: Regex,
      replacer: String,
      settings: I,
      code: String
    )(implicit codeProvider: CodeProvider[S, I, O]): Unit = {
    // Initially, execute the test defined.
    // This should be most expressed on the declaration.
    val firstResult =
      goTo(
        code = ArraySeq(code),
        searchSettings = settings,
        dependencyDownloaders = ArraySeq.empty
      ).map(_.toLineRange().value)

    // remove the select indicator @@ from the code.
    val codeWithoutSelectSymbol =
      code.replace(TestCodeUtil.SEARCH_INDICATOR, "")

    // find all matches
    val matches =
      finder
        .findAllMatchIn(codeWithoutSelectSymbol)
        .toList

    if (matches.isEmpty)
      fail(s"No matches found for regex '$finder' with replacement '$replacer'")

    // execute the same test on all matches
    matches foreach {
      matched =>
        // replace the `>>MyContract<<` with the `referenceReplacement` string.
        val newCode =
          StringUtil.replaceSubstring(
            string = codeWithoutSelectSymbol,
            start = matched.start,
            end = matched.end,
            replacement = replacer
          )

        def reportCodeOnFailure[T](test: => T): T =
          try
            test
          catch {
            case throwable: Throwable =>
              // report the code that failed
              fail(newCode, throwable)
          }

        // The code must have changed.
        // No point running test on the same code.
        codeWithoutSelectSymbol should not be newCode

        val newRanges =
          reportCodeOnFailure {
            goTo(
              code = ArraySeq(newCode),
              searchSettings = settings,
              dependencyDownloaders = ArraySeq.empty
            ).map(_.toLineRange().value)
          }

        // also check that the result is the same as the first ranges returned on declaration.
        reportCodeOnFailure {
          firstResult should contain theSameElementsAs newRanges
        }
    }
  }

  /**
   * Runs GoTo definition where `@@` is located
   * and expects the go-to location to be the text
   * between the symbols `>>...<<`.
   *
   * If the go-to symbols are not provided, then it expects an empty result.
   *
   * @param code The containing `@@` and `>>...<<` symbols.
   */
  private def goTo[S, I, O <: SourceLocation.GoTo](
      code: ArraySeq[String],
      searchSettings: I,
      dependencyDownloaders: ArraySeq[DependencyDownloader.Native]
    )(implicit codeProvider: CodeProvider[S, I, O]): List[O] = {
    val expectedLineRanges =
      TestCodeUtil.extractLineRangeInfo(code)

    // Remove all line-range indicators and keep the select indicator `@@`
    val codeWithoutLineRangeSymbols =
      code.map(_.replaceAll(">>|<<", ""))

    // Execute go-to definition.
    val (searchResultIterator, sourceCode, _) =
      TestCodeProvider[S, I, O](
        code = codeWithoutLineRangeSymbols,
        searchSettings = searchSettings,
        dependencyDownloaders = dependencyDownloaders
      )

    // Expect GoToLocations to also contain the fileURI
    val expectedGoToLocations =
      expectedLineRanges flatMap {
        case (ranges, code) =>
          val targetCode =
            sourceCode.find(_.code == code).value

          ranges map {
            range =>
              (targetCode.fileURI, range)
          }
      }

    // Convert to List for debugging
    val searchResultList =
      searchResultIterator.toList

    // For actual search result assert only the fileURI and line-ranges
    val actual =
      searchResultList map {
        result =>
          (result.parsed.fileURI, result.toLineRange().value)
      }

    // assert that the go-to definition jumps to all text between the go-to symbols << and >>
    // The error output of the above test is difficult to debug because `SourceIndex` only emits numbers.
    // For example: "LineRange(LinePosition(3, 16), LinePosition(3, 26)))) did not contain the same elements as Array()"
    // This print statement outputs a formatted compiler error message for better readability.
    TestCommon.tryOrPrintIndexer(
      codeBeingTested = code,
      code = searchResultList
    ) {
      actual should contain theSameElementsAs expectedGoToLocations
    }

    searchResultList
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
  def goToDefBuiltIn(
      code: String,
      expected: Option[String]): Assertion = {
    // Assert Node's AST
    goToDefBuiltInStrict(
      code = code,
      expected = expected
    )

    // Assert `SoftAST`
    goToDefBuiltInSoft(
      code = code,
      expected = expected
    )
  }

  def goToDefBuiltInStrict(
      code: String,
      expected: Option[String]): Assertion =
    // Assert Node's AST
    goToDependencyStrict[SourceCodeState.Parsed, GoToDefSetting, SourceLocation.GoToDefStrict](
      code = ArraySeq(code),
      expected = expected,
      downloader = BuiltInFunctionDownloader,
      settings = testGoToDefSetting
    )

  def goToDefBuiltInSoft(
      code: String,
      expected: Option[String]): Assertion =
    // Assert `SoftAST`
    goToDependencySoft[SourceCodeState.IsParsed, (SoftAST.type, GoToDefSetting), SourceLocation.GoToDefSoft](
      code = ArraySeq(code),
      expected = expected,
      downloader = BuiltInFunctionDownloader,
      settings = (SoftAST, testGoToDefSetting)
    )

  /**
   * Tests directly on the `std` native library.
   *
   * Runs go-to definition where `@@` is positioned, expecting
   * the resulting go-to definition to be in a std file
   * contained in the `std` library downloaded by native dependency
   * downloader [[StdInterfaceDownloader]].
   *
   * @param expected The expected dependency line, including the highlighted range `>>range to expect<<`.
   * @param code     The code with the search indicator '@@'.
   */
  def goToDefStd(expected: Option[String])(code: String*): Assertion =
    goToDefDependency(
      code = code.to(ArraySeq),
      expected = expected,
      downloader = StdInterfaceDownloader
    )

  /**
   * Tests directly on the `builtin` native library.
   *
   * Runs go-to type-definition where `@@` is positioned, expecting
   * the resulting go-to type-definition to be in a built-in file
   * contained in the `builtin` library downloaded by native dependency
   * downloader [[BuiltInFunctionDownloader]].
   *
   * @param expected The expected dependency line, including the highlighted range `>>range to expect<<`.
   * @param code     The code with the search indicator '@@'.
   */
  def goToTypeDefBuiltIn(expected: Option[String])(code: String*): Assertion =
    goToDependencyStrict[SourceCodeState.Parsed, Unit, SourceLocation.GoToTypeDef](
      code = code.to(ArraySeq),
      expected = expected,
      downloader = BuiltInFunctionDownloader,
      settings = ()
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
  def goToReferencesOnDependency(
      dependencyId: DependencyID,
      dependency: String,
      workspace: String,
      settings: GoToRefSetting = testGoToRefSetting): Unit =
    goTo[SourceCodeState.Parsed, GoToRefSetting, SourceLocation.GoToRefStrict](
      dependencyId = dependencyId,
      dependency = dependency,
      workspace = workspace,
      searchSettings = settings
    )

  def goToDefinitionOnDependency(
      dependencyId: DependencyID,
      dependency: String,
      workspace: String,
      setting: GoToDefSetting = testGoToDefSetting): Unit =
    goTo[SourceCodeState.Parsed, GoToDefSetting, SourceLocation.GoToDefStrict](
      dependencyId = dependencyId,
      dependency = dependency,
      workspace = workspace,
      searchSettings = setting
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
  private def goTo[S, I, O <: SourceLocation.GoTo](
      dependencyId: DependencyID,
      dependency: String,
      workspace: String,
      searchSettings: I
    )(implicit codeProvider: CodeProvider[S, I, O]): Unit = {
    implicit val clientLogger: ClientLogger = TestClientLogger
    implicit val file: FileAccess           = FileAccess.disk
    implicit val compiler: CompilerAccess   = CompilerAccess.ralphc

    // The indicator's gotta be in either the dependency or the workspace code.
    val isIndicatorInDependency =
      dependency contains TestCodeUtil.SEARCH_INDICATOR

    val (indicatorPosition, _, codeWithoutIndicatorMarker) =
      if (isIndicatorInDependency)
        TestCodeUtil.indicatorPositionOrFail(dependency) // maybe the indicator in dependency code
      else
        TestCodeUtil.indicatorPositionOrFail(workspace) // otherwise, the indicator must be in dependency code

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
        depCode = ArraySeq(dependencyCodeWithoutRangeMarkers)
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
        .value

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
        .value

    // use the selected fileURI to be the one with @@ indicator.
    val selectedFileURI =
      if (isIndicatorInDependency)
        dependencySourceFile.fileURI
      else
        sourceCode.fileURI

    val unCompiledWorkspace =
      WorkspaceState.UnCompiled(
        build = build,
        sourceCode = ArraySeq(sourceCode)
      )

    // run test
    val (searchResult, compiledWorkspace) =
      TestCodeProvider[S, I, O](
        line = indicatorPosition.line,
        character = indicatorPosition.character,
        selectedFileURI = selectedFileURI,
        searchSettings = searchSettings,
        workspace = unCompiledWorkspace
      )

    compiledWorkspace.sourceCode should have size 1
    compiledWorkspace.build.dependencies should have size 1

    val actualLineRanges = searchResult.value.flatMap(_.toLineRange()).toList
    actualLineRanges should contain theSameElementsAs expectedRanges

    TestWorkspace delete compiledWorkspace
    ()
  }

  /**
   * Runs go-to definition where @@ is positioned, expecting
   * the resulting go-to definition to be within a dependency workspace.
   *
   * @param code       The code with the search indicator '@@'.
   * @param expected   The expected dependency line, including the highlighted range `>>range to expect<<`.
   * @param downloader The native dependency to download, and to test on.
   *                   These must be of type [[DependencyDownloader.Native]]
   *                   as they can be written to `~/ralph-lsp`.
   *                   We don't want generated libraries being written to `~/ralph-lsp`.
   */
  private def goToDefDependency(
      code: ArraySeq[String],
      expected: Option[String],
      downloader: DependencyDownloader.Native): Assertion = {
    goToDependencyStrict[SourceCodeState.Parsed, GoToDefSetting, SourceLocation.GoToDefStrict](
      code = code,
      expected = expected,
      downloader = downloader,
      settings = testGoToDefSetting
    )

    goToDependencySoft[SourceCodeState.IsParsed, (SoftAST.type, GoToDefSetting), SourceLocation.GoToDefSoft](
      code = code,
      expected = expected,
      downloader = downloader,
      settings = (SoftAST, testGoToDefSetting)
    )
  }

  /**
   * Runs go-to definition where @@ is positioned, expecting
   * the resulting go-to definition to be within a dependency workspace.
   *
   * @param code       The code with the search indicator '@@'.
   * @param expected   The expected dependency line, including the highlighted range `>>range to expect<<`.
   * @param downloader The native dependency to download, and to test on.
   *                   These must be of type [[DependencyDownloader.Native]]
   *                   as they can be written to `~/ralph-lsp`.
   *                   We don't want generated libraries being written to `~/ralph-lsp`.
   */
  private def goToDependencyStrict[S, I, O <: SourceLocation.GoTo](
      code: ArraySeq[String],
      expected: Option[String],
      downloader: DependencyDownloader.Native,
      settings: I
    )(implicit provider: CodeProvider[S, I, O]): Assertion = {
    val codeWithoutGoToSymbols =
      code map TestCodeUtil.removeRangeSymbols

    // Execute go-to definition on strict-AST.
    val (searchResult, _, workspace) =
      TestCodeProvider[S, I, O](
        code = codeWithoutGoToSymbols,
        searchSettings = settings,
        dependencyDownloaders = ArraySeq(downloader)
      )

    assertGoToDependency(
      expected = expected,
      actual = searchResult,
      workspace = workspace,
      downloader = downloader
    )
  }

  /**
   * Runs go-to definition where @@ is positioned, expecting
   * the resulting go-to definition to be within a dependency workspace.
   *
   * @param code       The code with the search indicator '@@'.
   * @param expected   The expected dependency line, including the highlighted range `>>range to expect<<`.
   * @param downloader The native dependency to download, and to test on.
   *                   These must be of type [[DependencyDownloader.Native]]
   *                   as they can be written to `~/ralph-lsp`.
   *                   We don't want generated libraries being written to `~/ralph-lsp`.
   */
  private def goToDependencySoft[S, I, O <: SourceLocation.GoTo](
      code: ArraySeq[String],
      expected: Option[String],
      downloader: DependencyDownloader.Native,
      settings: I
    )(implicit provider: CodeProvider[S, I, O]): Assertion = {
    val codeWithoutGoToSymbols =
      code map TestCodeUtil.removeRangeSymbols

    // Execute go-to definition on SoftAST.
    val (searchResult, _, workspace) =
      TestCodeProvider[S, I, O](
        code = codeWithoutGoToSymbols,
        searchSettings = settings,
        dependencyDownloaders = ArraySeq(downloader)
      )

    assertGoToDependency(
      expected = expected,
      actual = searchResult,
      workspace = workspace,
      downloader = downloader
    )
  }

  /**
   * Asserts the go-to definition search result, expecting
   * the resulting go-to definition to be within a dependency workspace.
   *
   * @param expected   The expected dependency line, including the highlighted range `>>range to expect<<`.
   * @param actual     The actual search result.
   * @param workspace  The workspace where the search was executed.
   * @param downloader The native dependency to download, and to test on.
   *                   These must be of type [[DependencyDownloader.Native]]
   *                   as they can be written to `~/ralph-lsp`.
   *                   We don't want generated libraries being written to `~/ralph-lsp`.
   */
  private def assertGoToDependency(
      expected: Option[String],
      actual: Iterator[SourceLocation.GoTo],
      workspace: WorkspaceState.IsParsedAndCompiled,
      downloader: DependencyDownloader.Native): Assertion =
    expected match {
      case Some(expectedLineWithRange) =>
        val expectedLineWithoutRange =
          TestCodeUtil.removeRangeSymbols(expectedLineWithRange)

        val expectedResults =
          workspace
            .build
            .findDependency(downloader.dependencyID)
            .to(ArraySeq)
            .flatMap(_.sourceCode)
            .filter(_.code.contains(expectedLineWithoutRange)) // filter source-files that contain this code function.
            .flatMap {
              builtInFile =>
                // insert symbol >>..<<
                val codeWithRangeSymbols =
                  builtInFile
                    .code
                    .replace(expectedLineWithoutRange, expectedLineWithRange)

                if (builtInFile.code == codeWithRangeSymbols)
                  fail(s"Range symbols `>> <<` not provided in expected: \"$expectedLineWithRange\".")

                // compute the line range
                TestCodeUtil
                  .lineRangesOnly(codeWithRangeSymbols)
                  .map {
                    lineRange =>
                      (builtInFile.fileURI, lineRange)
                  }
            }

        if (expectedResults.isEmpty)
          fail(s"Could not find the expected line '$expectedLineWithRange'.")

        // For actual search result assert only the fileURI and line-ranges
        val actualResults =
          actual
            .toList
            .map {
              result =>
                (result.parsed.fileURI, result.toLineRange().value)
            }

        // assert that the go-to definition jumps to all text between the go-to symbols << and >>
        actualResults should contain theSameElementsAs expectedResults

      case None =>
        actual shouldBe empty
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
  private def apply[S, I, O](
      searchSettings: I,
      dependencyDownloaders: ArraySeq[DependencyDownloader.Native],
      code: ArraySeq[String]
    )(implicit provider: CodeProvider[S, I, O]): (Iterator[O], ArraySeq[SourceCodeState.IsCodeAware], WorkspaceState.IsParsedAndCompiled) = {
    implicit val clientLogger: ClientLogger = TestClientLogger
    implicit val file: FileAccess           = FileAccess.disk
    implicit val compiler: CompilerAccess   = CompilerAccess.ralphc

    val (atPosition, atSource, withoutAtSource) = {
      val (_withAt, withoutAt) =
        TestCodeUtil.extractAtInfo(code)

      // there must be one `@@` marker
      val (atPosition, atSource) =
        _withAt.value

      (atPosition, atSource, withoutAt)
    }

    // create a build file
    val build =
      TestBuild
        .genCompiledOK(dependencyDownloaders = dependencyDownloaders)
        .sample
        .value

    val withAtSourceCode =
      TestSourceCode
        .genOnDiskAndPersist(
          build = build,
          code = atSource
        )
        .sample
        .value

    val withoutAtSourceCode =
      withoutAtSource map {
        code =>
          TestSourceCode
            .genOnDiskAndPersist(
              build = build,
              code = code
            )
            .sample
            .value
      }

    val allSourceCode =
      withoutAtSourceCode :+ withAtSourceCode

    // create a workspace for all sources.
    val unCompiledWorkspace =
      WorkspaceState.UnCompiled(
        build = build,
        sourceCode = allSourceCode
      )

    // run completion at that line and character
    val (searchResult, compiledWorkspace) =
      TestCodeProvider(
        line = atPosition.line,
        character = atPosition.character,
        selectedFileURI = withAtSourceCode.fileURI,
        searchSettings = searchSettings,
        workspace = unCompiledWorkspace
      )

    compiledWorkspace.sourceCode should have size code.size.toLong

    // delete the workspace
    TestWorkspace delete compiledWorkspace

    // The workspace must contain `CodeAware` source files, meaning - it is read from disk.
    val codeAwareSource =
      compiledWorkspace.sourceCode.asInstanceOf[ArraySeq[SourceCodeState.IsCodeAware]]

    (searchResult.value, codeAwareSource, compiledWorkspace)
  }

  /**
   * Runs test on the given [[CodeProvider]], at the given line and character number.
   *
   * @param line            The target line number
   * @param character       The target character within the line
   * @param selectedFileURI The URI of the source-file where this search is to be executed.
   * @param workspace       The workspace to compile.
   * @return Suggestions and the created workspace.
   */
  private def apply[S, I, O](
      line: Int,
      character: Int,
      selectedFileURI: URI,
      searchSettings: I,
      workspace: WorkspaceState.UnCompiled
    )(implicit provider: CodeProvider[S, I, O],
      client: ClientLogger,
      file: FileAccess,
      compiler: CompilerAccess): (Either[CompilerMessage.Error, Iterator[O]], WorkspaceState.IsParsedAndCompiled) = {
    // parse and compile workspace
    val compiledWorkspace =
      Workspace.parseAndCompile(workspace)

    // execute code-provider.
    val result =
      CodeProvider.search(
        line = line,
        character = character,
        fileURI = selectedFileURI,
        workspace = compiledWorkspace,
        searchSettings = searchSettings
      )

    (result.value, compiledWorkspace)
  }

}
