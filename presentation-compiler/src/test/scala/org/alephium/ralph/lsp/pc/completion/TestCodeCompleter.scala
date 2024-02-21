package org.alephium.ralph.lsp.pc.completion

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.TestSourceCode
import org.alephium.ralph.lsp.pc.util.StringUtil
import org.alephium.ralph.lsp.pc.workspace.build.TestBuild
import org.alephium.ralph.lsp.pc.workspace.{TestWorkspace, Workspace, WorkspaceState}
import org.scalacheck.Gen
import org.scalatest.Assertions.fail
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._

import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

object TestCodeCompleter {

  /** Use this in your test-case for */
  private val completion_indicator =
    "@@"

  /**
   * Runs completion where `@@` is.
   *
   * For example: The following runs completion between the double quotes.
   * {{{
   *   TestCompleter(""" import "@@" """)
   * }}}
   */
  def apply(code: String): ArraySeq[Suggestion] = {
    val lines =
      StringUtil.codeLines(code)

    // find the line where @@ is located
    lines.zipWithIndex.find(_._1.contains(completion_indicator)) match {
      case Some((line, lineIndex)) =>
        // find the character where @@ is located
        val character =
          line.indexOf(completion_indicator)

        // remove @@
        val codeWithoutAtSymbol =
          code.replaceFirst(completion_indicator, "")

        // run completion at that line and character
        val (completion, workspace) =
          TestCodeCompleter(
            line = lineIndex,
            character = character,
            code = codeWithoutAtSymbol
          )

        // delete the workspace
        TestWorkspace delete workspace

        completion.value

      case None =>
        fail(s"Completion location indicator '$completion_indicator' not provided")
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
  private def apply(line: Int,
                    character: Int,
                    code: Gen[String]): (Either[CompilerMessage.Error, ArraySeq[Suggestion]], WorkspaceState.IsParsedAndCompiled) = {
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
      CodeCompleter.complete(
        line = line,
        character = character,
        fileURI = sourceCode.fileURI,
        workspace = compiledWorkspace
      )

    (completionResult.value, compiledWorkspace)
  }
}
