package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.sourcecode.GenSourceCode._
import org.alephium.ralph.lsp.pc.workspace.build.GenRalphc
import org.alephium.ralph.lsp.{FileIO, GenCode}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq

class SourceCodeParseSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "parse" should {
    "not access compiler" when {
      // compiler should not get accessed
      def testNoCompilerAccess(currentState: SourceCodeState) = {
        implicit val compiler: CompilerAccess = null
        implicit val file: FileAccess = null

        // execute parse
        val newState = SourceCode.parse(currentState)

        // expect same state to get returned without accessing the compiler
        newState shouldBe currentState
      }

      "source-code is already parsed" in {
        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        implicit val file: FileAccess =
          FileAccess.disk

        // Generate a source code file and parse it
        val parsed =
          GenSourceCode
            .genInitialised()
            .map(persist(_, GenCode.genGoodCode()))
            .map(SourceCode.parse)
            .map(_.asInstanceOf[SourceCodeState.Parsed])

        // Parse it again: Already parsed, so it should not access the compiler
        forAll(parsed) {
          sourceCode =>
            // test state
            testNoCompilerAccess(sourceCode)
            // delete file
            FileIO.delete(sourceCode.fileURI)
        }
      }

      "source-code is already compiled" in {
        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        implicit val file: FileAccess =
          FileAccess.disk

        // generate an already compiled source-code
        val parsed =
          GenSourceCode
            .genInitialised()
            .map(persist(_, GenCode.genGoodCode()))
            .map(SourceCode.parse)
            .map(_.asInstanceOf[SourceCodeState.Parsed])

        forAll(parsed, GenRalphc.genCompilerOptions()) {
          case (parsed, compilerOptions) =>

            // compile the source-code
            val compiledResult =
              SourceCode
                .compile(
                  sourceCode = ArraySeq(parsed),
                  dependency = None,
                  compilerOptions = compilerOptions,
                )

            // compilation successful
            val compiled =
              compiledResult
                .value
                .asInstanceOf[ArraySeq[SourceCodeState.Compiled]]

            // it should contain only one result
            compiled should have size 1

            // Compile again: Already compiled, so it should not access the compiler.
            testNoCompilerAccess(compiled.head)

            // delete file
            FileIO.delete(compiled.head.fileURI)
        }
      }
    }
  }
}
