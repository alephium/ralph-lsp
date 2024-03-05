package org.alephium.ralph.lsp.pc.search

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.{CodePosition, CodeRange, CompilerMessage}
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.search.gotodef.data.GoToLocation
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, TestSourceCode}
import org.alephium.ralph.lsp.pc.util.StringUtil
import org.alephium.ralph.lsp.pc.workspace.build.TestBuild
import org.alephium.ralph.lsp.pc.workspace.{TestWorkspace, Workspace, WorkspaceState}
import org.scalacheck.Gen
import org.scalatest.Assertions.fail
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers._

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

object TestCodeProvider {

  /** Use this in your test-case for */
  private val SEARCH_INDICATOR =
    "@@"

  /**
   * Runs completion where `@@` is.
   *
   * For example: The following runs completion between the double quotes.
   * {{{
   *   TestCompleter(""" import "@@" """)
   * }}}
   */
  def apply[A](code: String)(implicit provider: CodeProvider[A]): (ArraySeq[A], SourceCodeState.IsCodeAware) = {
    val lines =
      StringUtil.codeLines(code)

    // find the line where @@ is located
    lines.zipWithIndex.find(_._1.contains(SEARCH_INDICATOR)) match {
      case Some((line, lineIndex)) =>
        // find the character where @@ is located
        val character =
          line.indexOf(SEARCH_INDICATOR)

        // remove @@
        val codeWithoutAtSymbol =
          code.replaceFirst(SEARCH_INDICATOR, "")

        // run completion at that line and character
        val (searchResult, workspace) =
          TestCodeProvider(
            line = lineIndex,
            character = character,
            code = codeWithoutAtSymbol
          )

        workspace.sourceCode should have size 1

        // delete the workspace
        TestWorkspace delete workspace

        (searchResult.value, workspace.sourceCode.head.asInstanceOf[SourceCodeState.IsCodeAware])

      case None =>
        fail(s"Completion location indicator '$SEARCH_INDICATOR' not provided")
    }
  }

  /**
   * Runs GoTo definition where `@@` is located
   * and expects the go-to location to be the text
   * between the symbols `<<...>>`.
   *
   * If the go-to symbols are not provided, then it expects empty result.
   *
   * @param code The containing `@@` and `<<...>>` symbols.
   */
  def goTo(code: String): Unit = {
    val lines =
      StringUtil
        .codeLines(code)
        .zipWithIndex

    val goToStart = lines.find(_._1.contains(">>"))
    val goToEnd = lines.find(_._1.contains("<<"))

    // find the line where the go-to symbols >> and << are located
    (goToStart, goToEnd) match {
      case (Some((startLine, startLineIndex)), Some((endLine, endLineIndex))) =>
        // Code range should be where << and >> are located
        val expectedCodeRange =
          CodeRange(
            CodePosition(startLineIndex, startLine.indexOf(">>")),
            CodePosition(endLineIndex, endLine.replaceFirst(">>", "").indexOf("<<"))
          )

        // remove << and >>
        val codeWithoutGoToSymbols =
          code
            .replaceFirst(">>", "")
            .replaceFirst("<<", "")

        // Execute go-to definition.
        val (searchResult, sourceCode) =
          TestCodeProvider[GoToLocation](codeWithoutGoToSymbols)

        searchResult should have size 1

        // assert that the go-to definition jumps to the text between the go-to symbols << and >>
        searchResult.head shouldBe
          GoToLocation(
            uri = sourceCode.fileURI,
            codeRange = expectedCodeRange
          )

      case (None, None) =>
        // Expect empty result because no go-to symbols << and >> were provided.
        val (searchResult, _) =
          TestCodeProvider[GoToLocation](code)

        searchResult shouldBe empty

      case (_, _) =>
        fail(s"GoTo location indicator '<< and >>' not provided")
    }
  }

  /**
   * Run test completion.
   *
   * @param line      The target line number
   * @param character The target character within the line
   * @param code      The code to run completion on.
   * @return Suggestions and the created workspace.
   */
  private def apply[A](line: Int,
                       character: Int,
                       code: Gen[String])(implicit provider: CodeProvider[A]): (Either[CompilerMessage.Error, ArraySeq[A]], WorkspaceState.IsParsedAndCompiled) = {
    implicit val clientLogger: ClientLogger = TestClientLogger
    implicit val file: FileAccess = FileAccess.disk
    implicit val compiler: CompilerAccess = CompilerAccess.ralphc

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
      TestSourceCode.genOnDiskAndPersist(
        fileURI = sourceFile,
        code = code.sample.get
      ).sample.get

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
