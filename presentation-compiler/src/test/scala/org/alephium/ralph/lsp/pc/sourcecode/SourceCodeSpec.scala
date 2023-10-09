package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.sourcecode.GenSourceCode._
import org.alephium.ralph.lsp.GenCommon._
import org.alephium.ralph.lsp.compiler.message.error.StringError
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.net.URI

class SourceCodeSpec extends AnyWordSpec with Matchers with MockFactory with ScalaCheckDrivenPropertyChecks {

  "initialise" should {
    "fetch all source file names" in {
      forAll(genFolderAndFileURIs()) {
        case (folder, fileURIs) =>
          implicit val compiler: CompilerAccess =
            mock[CompilerAccess]

          // compiler is access to fetch the source files from disk
          (compiler.getSourceFiles _)
            .expects(folder)
            .returns(Right(fileURIs))
            .once()

          val expectedState =
            fileURIs map SourceCodeState.OnDisk

          val actualState =
            SourceCode.initialise(folder).value

          actualState should contain allElementsOf expectedState
      }
    }
  }

  "parse" should {
    "not access compiler" when {
      // compiler should not get accessed
      def testNoCompilerAccess(currentState: SourceCodeState) = {
        implicit val compiler: CompilerAccess =
          null

        // execute parse
        val newState = SourceCode.parse(currentState)

        // expect same state to get returned without accessing the compiler
        newState shouldBe currentState
      }

      "source-code is already parsed" in {
        // State: source test.ral is already parsed
        val parsedState =
          SourceCodeState.Parsed(
            fileURI = URI.create("./test.ral"),
            code = "blah",
            contracts = Seq.empty
          )

        testNoCompilerAccess(parsedState)
      }

      "source-code is already compiled" in {
        // State: source test.ral is already compiled
        val compiledState =
          SourceCodeState.Compiled(
            fileURI = URI.create("./test.ral"),
            code = "blah",
            compiledCode = Seq.empty,
            parsed = null
          )

        testNoCompilerAccess(compiledState)
      }
    }

    "return errored state" when {
      "un-compiled state returns an error" in {
        val fileURI =
          URI.create("some_file")

        val code =
          "code"

        // error returned from the compiler
        val expectedError =
          StringError("some error")

        implicit val compiler: CompilerAccess =
          mock[CompilerAccess]

        // parseContracts should only be called once (not go into recursion) and error is reported to the user
        (compiler.parseContracts _)
          .expects(code)
          .returns(Left(expectedError))
          .once()

        // code is un-compiled
        val state =
          SourceCodeState.UnCompiled(
            fileURI = fileURI,
            code = code
          )

        val newState = SourceCode.parse(state)

        // expect error state with the origin code
        newState shouldBe
          SourceCodeState.ErrorSource(
            fileURI = fileURI,
            code = code,
            errors = Seq(expectedError),
            previous = None
          )
      }

      "existing failed state, returns another failed state for unable to access file on disk" in {
        forAll(genFailedAccess()) {
          failedAccessState =>
            implicit val compiler: CompilerAccess =
              CompilerAccess.ralphc

            // the fileURI does not exists, so expect a failed state.
            val newState = SourceCode.parse(failedAccessState)

            // expect error state with the origin code
            newState.fileURI shouldBe failedAccessState.fileURI
            newState shouldBe a[SourceCodeState.ErrorAccess]
        }
      }
    }

    "transform to source-code to parsed state" when {
      "failed access state returns a success" in {
        forAll(genFailedAccess()) {
          failedState =>
            implicit val compiler: CompilerAccess =
              mock[CompilerAccess]

            val code = "some code"

            // Code is read from disk (once)
            (compiler.getSourceCode _)
              .expects(failedState.fileURI)
              .returns(Right(code))
              .once()

            // Code is parsed (once)
            (compiler.parseContracts _)
              .expects(code)
              .returns(Right(Seq.empty))
              .once()

            // successfully parses the code
            val newState = SourceCode.parse(failedState)

            // expect error state with the origin code
            newState shouldBe
              SourceCodeState.Parsed(
                fileURI = failedState.fileURI,
                code = code,
                contracts = Seq.empty
              )
        }
      }
    }

    "return the same state" when {
      "source code is errored" in {
        forAll(genErrorSource()) {
          failedState =>
            // compiler does not get accessed
            implicit val compiler: CompilerAccess =
              null

            val newState = SourceCode.parse(failedState)

            // expect error state remains unchanged.
            // The code didn't change so will the error.
            newState shouldBe failedState
        }
      }
    }
  }
}
