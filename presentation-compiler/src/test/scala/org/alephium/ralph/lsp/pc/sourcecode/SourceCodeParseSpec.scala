package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.error.TestError
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.sourcecode.TestSourceCode._
import org.alephium.ralph.lsp.pc.workspace.build.TestRalphc
import org.alephium.ralph.lsp.{TestCode, TestFile}
import org.scalatest.EitherValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
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
          TestSourceCode
            .genInitialised()
            .map(persist(_, TestCode.genGoodCode()))
            .map(SourceCode.parse)
            .map(_.asInstanceOf[SourceCodeState.Parsed])

        // Parse it again: Already parsed, so it should not access the compiler
        forAll(parsed) {
          sourceCode =>
            // test state
            testNoCompilerAccess(sourceCode)
            // delete file
            TestSourceCode.delete(sourceCode)
        }
      }

      "source-code is already compiled" in {
        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        implicit val file: FileAccess =
          FileAccess.disk

        // generate an already compiled source-code
        val parsed =
          TestSourceCode
            .genInitialised()
            .map(persist(_, TestCode.genGoodCode()))
            .map(SourceCode.parse)
            .map(_.asInstanceOf[SourceCodeState.Parsed])

        forAll(parsed, TestRalphc.genCompilerOptions()) {
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
            TestSourceCode.delete(compiled.head)
        }
      }
    }

    "successfully parse" when {
      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

      implicit val file: FileAccess =
        FileAccess.disk

      "source-code in state OnDisk, UnCompiled and ErrorAccess" in {
        forAll(TestSourceCode.genOnDiskAndPersist(), TestError.genError()) {
          case ((onDisk, goodCode), error) =>

            /**
             * Generate an [[SourceCodeState.UnCompiled]] and [[SourceCodeState.ErrorAccess]] for this onDisk state.
             *
             * The end result should be the same, the code should still successfully parse for all these states
             * because the code on-disk is "Good Code".
             * */

            val unCompiled =
              SourceCodeState.UnCompiled(
                fileURI = onDisk.fileURI,
                code = goodCode
              )

            val errorAccess =
              SourceCodeState.ErrorAccess(
                fileURI = onDisk.fileURI,
                error = error
              )

            // run tests for all states
            val allStates =
              List(onDisk, unCompiled, errorAccess)

            // all states (errors or onDisk) should always succeed because the onDisk code is valid and parse-able.
            allStates foreach {
              currentState =>
                // execute parse
                val actual = SourceCode.parse(currentState)
                // successfully parsed
                actual shouldBe
                  SourceCodeState.Parsed(
                    fileURI = currentState.fileURI,
                    code = goodCode,
                    ast = compiler.parseContracts(currentState.fileURI, goodCode).value
                  )

                // read the code written on disk
                val sourceCodeOnDisk =
                  TestFile.readAll(actual.fileURI)
                // it should be the same as goodCode
                sourceCodeOnDisk shouldBe goodCode
            }

            // clear persisted source-code
            TestSourceCode.delete(onDisk)
        }
      }
    }

    "not parse and not perform disk-IO or compiler-IO" when {
      "source-code already in ErrorSource state" in {
        // compiler and disk should not be accessed
        implicit val compiler: CompilerAccess = null
        implicit val file: FileAccess = null

        // ErrorSource state means the source-code has already been parsed and it previously errored.
        // Re-parsing will not yield a different result so expect no disk-IO or compiler-IO
        forAll(TestSourceCode.genErrorSource()) {
          error =>
            SourceCode.parse(error) shouldBe error
        }
      }
    }

    "report source-code parse error" in {
      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

      implicit val file: FileAccess =
        FileAccess.disk

      forAll(TestSourceCode.genOnDiskAndPersist(code = TestCode.genBadCode())) {
        case (onDisk, code) =>
          // execute parse
          val newState = SourceCode.parse(onDisk)

          // expect error state is returned
          newState shouldBe
            SourceCodeState.ErrorSource(
              fileURI = onDisk.fileURI,
              code = code,
              errors = Seq(compiler.parseContracts(onDisk.fileURI, code).left.value),
              previous = None
            )

          TestSourceCode.delete(onDisk)
      }
    }
  }
}
