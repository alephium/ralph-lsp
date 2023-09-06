package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.pc.compiler.CompilerAccess
import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.pc.data.FileError
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.TryValues._

import java.net.URI
import java.nio.file.{Files, Paths}

class SourceCodeSpec extends AnyWordSpec with Matchers with MockFactory {

  "initialise" should {
    "not throw exception" in {
      val nonExistingDir = Paths.get("initialise_test")
      // assert that it does not throw exception
      SourceCode.initialise(nonExistingDir).success.value shouldBe empty
    }

    "parse all ralph file names from disk" in {
      val dir = Files.createTempDirectory("initialise_test")
      val one = Files.createFile(dir.resolve("one.ral"))
      val two = Files.createFile(dir.resolve("two.ral"))

      SourceCode.initialise(dir).success.value shouldBe
        Seq(
          SourceCodeState.OnDisk(one.toUri),
          SourceCodeState.OnDisk(two.toUri)
        )
    }
  }

  "parse" should {
    "not access compiler" when {
      "the source-code is already parsed" in {
        // compile should get accessed
        implicit val compiler = mock[CompilerAccess]

        // source test.ral is already parsed
        val currentState =
          SourceCodeState.Parsed(
            fileURI = URI.create("./test.ral"),
            code = "blah",
            parsedAST = Ast.MultiContract(Seq.empty, None)
          )

        // execute parse
        val newState = SourceCode.parse(currentState)

        // expect same state to get returned without accessing the compiler
        currentState shouldBe newState
      }

      "the source-code is already compiled" in {
        // compile should get accessed
        implicit val compiler = mock[CompilerAccess]

        // source test.ral is already compiled
        val currentState =
          SourceCodeState.Compiled(
            fileURI = URI.create("./test.ral"),
            code = "blah",
            compiledCode = Seq.empty,
            previousState = null
          )

        // execute parse
        val newState = SourceCode.parse(currentState)

        // expect same state to get returned without accessing the compiler
        currentState shouldBe newState
      }
    }

    "return errored state" when {
      "un-compiled returns an error" in {
        val fileURI =
          URI.create("some_file")

        val code =
          "code"

        val expectedError =
          FileError("some error")

        implicit val compiler = mock[CompilerAccess]

        (compiler.parseCode _).expects(code).returns(Left(expectedError))

        val state =
          SourceCodeState.UnCompiled(
            fileURI = fileURI,
            code = code
          )

        val newState = SourceCode.parse(state)

        newState shouldBe
          SourceCodeState.Errored(
            fileURI = fileURI,
            code = code,
            errors = Seq(expectedError),
            previous = None
          )
      }
    }

    "parsed state" when {
      "un-compiled returns a success" in {
        val fileURI =
          URI.create("some_file")

        val code =
          "code"

        val expected =
          Ast.MultiContract(Seq.empty, None)

        implicit val compiler = mock[CompilerAccess]

        (compiler.parseCode _).expects(code).returns(Right(expected))

        val state =
          SourceCodeState.UnCompiled(
            fileURI = fileURI,
            code = code
          )

        val newState = SourceCode.parse(state)

        newState shouldBe
          SourceCodeState.Parsed(
            fileURI = fileURI,
            code = code,
            parsedAST = expected
          )
      }
    }
  }
}
