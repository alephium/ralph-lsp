// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.{TestCode, TestFile}
import org.alephium.ralph.lsp.GenExtension.GenExtensionsImplicits
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.{LinePosition, LineRange}
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.access.util.TestCodeUtil
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, TestSourceCode}
import org.alephium.ralph.lsp.pc.sourcecode.TestSourceCode._
import org.alephium.ralph.lsp.pc.workspace.build.{BuildState, TestBuild}
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers._
import org.scalatest.OptionValues._

import java.net.URI
import scala.collection.immutable.ArraySeq

/** [[Workspace]] related test functions */
object TestWorkspace {

  def genCreated(directory: Gen[URI] = TestFile.genFolderURI()): Gen[WorkspaceState.Created] =
    directory map WorkspaceState.Created.apply

  /**
   * Generate a [[org.alephium.ralph.lsp.pc.workspace.WorkspaceState.Created]] workspace with OnDisk source code.
   *
   * @param directory The workspace directory
   * @param persist   When true, persist workspace directory and source code.
   */
  def genCreatedWithSourceCode(
      directory: Gen[URI] = TestFile.genFolderURI(),
      persist: Boolean = false): Gen[(WorkspaceState.Created, List[SourceCodeState.OnDisk])] =
    for {
      workspace  <- TestWorkspace.genCreated(directory)
      sourceCode <- Gen.listOfMax()(TestSourceCode.genOnDiskForRoot(workspace.workspaceURI))
    } yield
      if (persist)
        (TestWorkspace.persist(workspace), TestSourceCode.persistAll(sourceCode))
      else
        (workspace, sourceCode)

  /**
   * Generates a parsed workspace from the provided source code.
   *
   * @param code The source code to parse.
   * @return A parsed workspace.
   */
  def genParsedOK(
      code: String*
    )(implicit compiler: CompilerAccess,
      file: FileAccess,
      client: ClientLogger): Gen[WorkspaceState.Parsed] =
    genParsed(code: _*).map(_.asInstanceOf[WorkspaceState.Parsed])

  def genParsedOK(
      build: Gen[BuildState.Compiled],
      code: Seq[String]
    )(implicit compiler: CompilerAccess,
      file: FileAccess): Gen[WorkspaceState.Parsed] =
    genParsed(
      build = build,
      code = code
    ).map(_.asInstanceOf[WorkspaceState.Parsed])

  /**
   * Generates a parsed or errored workspace from the provided source code.
   *
   * @param code The source code to parse.
   * @return The workspace containing the parser result.
   */
  def genParsed(
      code: String*
    )(implicit compiler: CompilerAccess,
      file: FileAccess,
      client: ClientLogger): Gen[WorkspaceState.IsParsed] =
    genParsed(
      build = TestBuild.genCompiledOK(),
      code = code.toSeq
    )

  /**
   * Generates a parsed or errored workspace from the provided source code.
   *
   * @param code The source code to parse.
   * @return The workspace containing the parser result.
   */
  def genParsed(
      build: Gen[BuildState.Compiled],
      code: Seq[String]
    )(implicit compiler: CompilerAccess,
      file: FileAccess): Gen[WorkspaceState.IsParsed] =
    genUnCompiled(
      build = build,
      code = code
    ) map {
      unCompiled =>
        // try parsing the workspace
        val workspace =
          Workspace.parse(unCompiled)

        // it should contains the all the source file
        workspace.sourceCode.length shouldBe code.size

        workspace
    }

  def genCompiledOK(
      code: String*
    )(implicit compiler: CompilerAccess,
      file: FileAccess,
      client: ClientLogger): Gen[WorkspaceState.Compiled] =
    genCompiled(code: _*).map(_.asInstanceOf[WorkspaceState.Compiled])

  def genCompiled(
      code: String*
    )(implicit compiler: CompilerAccess,
      file: FileAccess,
      client: ClientLogger): Gen[WorkspaceState.IsCompiled] =
    genParsedOK(code: _*) map {
      parsed =>
        Workspace.compile(parsed)
    }

  def genCompiledOK(
      build: Gen[BuildState.Compiled],
      code: Seq[String]
    )(implicit compiler: CompilerAccess,
      file: FileAccess,
      logger: ClientLogger): Gen[WorkspaceState.Compiled] =
    genCompiled(
      build = build,
      code = code
    ).map(_.asInstanceOf[WorkspaceState.Compiled])

  def genCompiled(
      build: Gen[BuildState.Compiled],
      code: Seq[String]
    )(implicit compiler: CompilerAccess,
      file: FileAccess,
      logger: ClientLogger): Gen[WorkspaceState.IsCompiled] =
    genParsedOK(
      build = build,
      code = code
    ) map {
      parsed =>
        Workspace.compile(parsed)
    }

  def genUnCompiled(
      code: String*
    )(implicit compiler: CompilerAccess,
      file: FileAccess,
      client: ClientLogger): Gen[WorkspaceState.UnCompiled] =
    genUnCompiled(
      build = TestBuild.genCompiledOK(),
      code = code.toSeq
    )

  /**
   * Generates an un-compiled workspace from the provided source code.
   *
   * @param code The source code to create an un-compiled workspace for.
   * @return An un-compiled workspace.
   */
  def genUnCompiled(
      build: Gen[BuildState.Compiled],
      code: Seq[String]): Gen[WorkspaceState.UnCompiled] =
    build map {
      build =>
        val sourceFiles =
          code
            .zipWithIndex
            .map {
              case (code, index) =>
                SourceCodeState.UnCompiled(
                  fileURI = build.contractURI.resolve(s"file$index.ral"),
                  code = code
                )
            }
            .to(ArraySeq)

        // create a workspace for the source files above
        val unCompiled =
          WorkspaceState.UnCompiled(
            build = build,
            sourceCode = sourceFiles
          )

        // it should contains the all the source file
        unCompiled.sourceCode.length shouldBe code.size

        unCompiled
    }

  /**
   * Extracts the location of the `@@` marker from the given source code,
   * and maps each source code snippet to its corresponding source code state within the workspace.
   * Only one `@@` marker is expected in the input. Any `>><<` markers will be removed.
   *
   * @note All workspace code must be unique. To create duplicates, use comments.
   * @param codeWithMarkers The source code lines containing at most one `@@` marker.
   * @param workspaces      The workspaces to which the source code lines belong.
   * @return A tuple consisting of:
   *         - A pair of the `@@` marker's position and its associated source code state.
   *         - Other source code states.
   */
  def extractAtInfo(
      codeWithMarkers: ArraySeq[String],
      workspaces: ArraySeq[WorkspaceState.IsParsedAndCompiled]): ((LinePosition, SourceCodeState.IsCodeAware), ArraySeq[SourceCodeState.IsCodeAware]) = {
    val (withAt, withoutAt) =
      TestCodeUtil.extractAtInfo(codeWithMarkers)

    val workspaceSourceCode =
      workspaces
        .flatMap(_.sourceCode)
        .asInstanceOf[ArraySeq[SourceCodeState.IsCodeAware]]

    val dependencySourceCode =
      workspaces
        .flatMap(_.build.dependencies.flatMap(_.sourceCode))
        .distinct
        .asInstanceOf[ArraySeq[SourceCodeState.IsCodeAware]]

    val allSource =
      workspaceSourceCode ++ dependencySourceCode

    val atInfo =
      withAt map {
        case (position, code) =>
          val source = allSource.find(_.code == code)
          (position, source.value)
      }

    val withoutAtInfo =
      withoutAt map {
        code =>
          val found = allSource.find(_.code == code)
          found.value
      }

    (atInfo.value, withoutAtInfo)
  }

  /**
   * Extracts the line range `>><<` marker from information the given source code,
   * and maps each source code snippet to its corresponding source code state within the workspace.
   * Only the `>><<` markers are expected in the input. Any `@@` markers will be removed.
   *
   * @note All workspace code must be unique. To create duplicates, use comments.
   * @param codeWithMarkers The source code lines containing the range `>><<` markers.
   * @param workspaces      The workspaces to which the source code lines belong.
   * @return A tuple consisting of:
   *         - A pair of the range marker positions and its associated source code state.
   *         - Other source code states.
   */
  def extractLineRange(
      codeWithMarkers: ArraySeq[String],
      workspaces: ArraySeq[WorkspaceState.IsParsedAndCompiled]): ArraySeq[(Array[LineRange], SourceCodeState.IsCodeAware)] = {
    val rangeInfo =
      TestCodeUtil.extractLineRangeInfo(codeWithMarkers)

    val workspaceSourceCode =
      workspaces
        .flatMap(_.sourceCode)
        .asInstanceOf[ArraySeq[SourceCodeState.IsCodeAware]]

    val dependencySourceCode =
      workspaces
        .flatMap(_.build.dependencies.flatMap(_.sourceCode))
        .distinct
        .asInstanceOf[ArraySeq[SourceCodeState.IsCodeAware]]

    val allSource =
      workspaceSourceCode ++ dependencySourceCode

    rangeInfo map {
      case (position, code) =>
        val source = allSource.find(_.code == code)
        (position, source.value)
    }
  }

  def persist[W <: WorkspaceState.IsSourceAware](
      workspace: W,
      code: Gen[String] = TestCode.genGoodCode()): W = {
    TestFile.createDirectories(workspace.workspaceURI)
    persistAll(
      sourceCode = workspace.sourceCode,
      code = code
    )
    workspace
  }

  def persist(workspace: WorkspaceState.Created): WorkspaceState.Created = {
    TestFile.createDirectories(workspace.workspaceURI)
    workspace
  }

  /** Delete the workspace folder and all its content */
  def delete[W <: WorkspaceState](workspace: W): W = {
    TestFile.deleteAll(workspace.workspaceURI)
    workspace
  }

}
