// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search

import org.alephium.ralph.lsp.{TestCommon, TestFile}
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.LineRange
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.access.util.TestCodeUtil
import org.alephium.ralph.lsp.pc.{PCState, PCStates}
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, TestSourceCode}
import org.alephium.ralph.lsp.pc.workspace.{TestWorkspace, Workspace, WorkspaceState}
import org.alephium.ralph.lsp.pc.workspace.build.{TestBuild, TestRalphc}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.{DependencyID, TestDependency}
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.utils.IsCancelled
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers._
import org.scalatest.time.SpanSugar._
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._

import java.net.URI
import java.nio.file.Paths
import scala.collection.immutable.ArraySeq
import scala.concurrent.ExecutionContext

object TestMultiCodeProvider extends ScalaFutures {

  /**
   * Runs the [[org.alephium.ralph.lsp.pc.search.gotodef.multi.GoToDefMultiCodeProvider]] on the given workspaces,
   * which contains the selection indicator `@@` and may also include the result indicator `>><<`.
   *
   * @param enableSoftParser Whether to use the soft parser.
   * @param workspaces       The source code for the workspaces.
   * @return The line range pairs pointing to the resolved locations.
   */
  def goToDefMulti(
      enableSoftParser: Boolean = false
    )(workspaces: ArraySeq[String]*
    )(implicit logger: ClientLogger,
      file: FileAccess,
      compiler: CompilerAccess,
      ec: ExecutionContext): ArraySeq[(URI, LineRange)] =
    goToDefMultiWithDependency(enableSoftParser = enableSoftParser)(
      dependencyID = DependencyID.Std,
      dependency = ArraySeq.empty,
      workspaces = workspaces: _*
    )

  /**
   * Runs the [[org.alephium.ralph.lsp.pc.search.gotodef.multi.GoToDefMultiCodeProvider]] on the given workspaces and dependency,
   * which contains the selection indicator `@@` and may also include the result indicator `>><<`.
   *
   * @param enableSoftParser Whether to use the soft parser.
   * @param dependencyID     The ID to assign to the created dependency.
   * @param dependency       The source code for the dependency.
   * @param workspaces       The source code for the workspaces.
   * @return The line range pairs pointing to the resolved locations.
   */
  def goToDefMultiWithDependency(
      enableSoftParser: Boolean = false
    )(dependencyID: DependencyID,
      dependency: ArraySeq[String],
      workspaces: ArraySeq[String]*
    )(implicit logger: ClientLogger,
      file: FileAccess,
      compiler: CompilerAccess,
      ec: ExecutionContext): ArraySeq[(URI, LineRange)] =
    goTo[Unit, SourceLocation.GoToDef](
      enableSoftParser = enableSoftParser,
      settings = (),
      dependencyID = dependencyID,
      dependency = dependency.to(ArraySeq),
      workspaces = workspaces.to(ArraySeq)
    )

  /**
   * Runs the [[MultiCodeProvider]] on the given workspaces and dependency,
   * which contains the selection indicator `@@` and may also include the result indicator `>><<`.
   *
   * @param enableSoftParser Whether to use the soft parser.
   * @param settings         Settings for the [[MultiCodeProvider]].
   * @param dependencyID     The ID to assign to the created dependency.
   * @param dependency       The source code for the dependency.
   * @param workspaces       The source code for the workspaces.
   * @tparam I [[MultiCodeProvider]]s settings type.
   * @tparam O [[MultiCodeProvider]]s output type.
   * @return The line range pairs pointing to the resolved locations.
   */
  private def goTo[I, O <: SourceLocation.GoTo](
      enableSoftParser: Boolean,
      settings: I,
      dependencyID: DependencyID,
      dependency: ArraySeq[String],
      workspaces: ArraySeq[ArraySeq[String]]
    )(implicit provider: MultiCodeProvider[I, O],
      logger: ClientLogger,
      file: FileAccess,
      compiler: CompilerAccess,
      ec: ExecutionContext): ArraySeq[(URI, LineRange)] = {
    // For test-cases, set the dependency paths to be the same for all workspaces.
    // This is because a majority of test-cases will require this behaviour.
    val dependencyAbsolutePath =
      Paths.get(TestFile.genFolderURI().sample.value).toString

    // Build a dependency downloader that returns the given dependency-code.
    val dependencyDownloader =
      if (dependency.nonEmpty)
        ArraySeq(
          TestDependency.buildDependencyDownloader(
            depId = dependencyID,
            depCode = dependency map TestCodeUtil.clearTestMarkers
          )
        )
      else
        ArraySeq.empty

    // create a compiled workspace from the given source-code and the dependency downloader.
    val compiledWorkspace =
      workspaces map {
        workspaceCode =>
          // create a build file
          val build =
            TestBuild
              .genCompiledOK(
                config = TestRalphc.genRalphcParsedConfig(dependenciesFolderName = Some(dependencyAbsolutePath)),
                dependencyDownloaders = dependencyDownloader
              )
              .sample
              .value

          // generate parsed source-code states for all text sources.
          val sourceCode =
            TestSourceCode.genParsed(
              build = build,
              code = workspaceCode map TestCodeUtil.clearTestMarkers: _*
            )

          // create an un-compiled workspace for all sources.
          val unCompiledWorkspace =
            WorkspaceState.UnCompiled(
              build = build,
              sourceCode = sourceCode.to(ArraySeq)
            )

          // create a compiled workspace
          Workspace.parseAndCompile(unCompiledWorkspace)
      }

    // all dependency and workspaces source code
    val allCodeWithMarkers =
      workspaces.flatten ++ dependency

    // `@@` marker information
    val ((atLocation, codeWithAt), _) =
      TestWorkspace.extractAtInfo(
        codeWithMarkers = allCodeWithMarkers,
        workspaces = compiledWorkspace
      )

    // `>><<` marker information
    val lineRangeInfo =
      TestWorkspace.extractLineRange(
        codeWithMarkers = allCodeWithMarkers,
        workspaces = compiledWorkspace
      )

    // Create presentation-compiler states for each workspace
    val pcStates =
      compiledWorkspace map {
        workspace =>
          PCState(
            workspace = workspace,
            buildErrors = None,
            tsErrors = None
          )
      }

    // Execute the multi-code provider on the `PCState`s
    val result =
      provider
        .search(
          fileURI = codeWithAt.fileURI,
          line = atLocation.line,
          character = atLocation.character,
          enableSoftParser = enableSoftParser,
          // format: off
          isCancelled = IsCancelled(() => false), // format: on
          pcStates = PCStates(pcStates),
          settings = settings
        )
        .futureValue(Timeout(5.seconds))

    // Delete the created workspaces
    compiledWorkspace foreach TestWorkspace.delete

    // Error is not expected
    val searchResult = result.value

    // Expected search result.
    // Assert only the fileURI and line-ranges.
    val expected =
      lineRangeInfo flatMap {
        case (ranges, code) =>
          ranges map {
            range =>
              (code.fileURI, range)
          }
      }

    // Actual search result.
    val actual =
      searchResult map {
        result =>
          (result.parsed.fileURI, result.toLineRange().value)
      }

    // assert that the go-to definition jumps to all text between the go-to symbols << and >>
    // The error output of the above test is difficult to debug because `SourceIndex` only emits numbers.
    // For example: "LineRange(LinePosition(3, 16), LinePosition(3, 26)))) did not contain the same elements as Array()"
    // This print statement outputs a formatted compiler error message for better readability.
    TestCommon.tryOrPrintIndexer(
      codeBeingTested = workspaces.flatten,
      code = searchResult
    ) {
      actual should contain theSameElementsAs expected
    }

    actual
  }

}
