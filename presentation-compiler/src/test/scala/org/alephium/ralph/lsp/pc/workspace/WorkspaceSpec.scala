package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.sourcecode.{GenSourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.GenWorkspace._
import org.alephium.ralph.lsp.GenCommon._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq

class WorkspaceSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks with MockFactory {

  "initialise" when {
    "all files are successfully read" should {
      "start workspace in un-compiled state" in {
        forAll(genWorkspaceBuild(), Gen.listOf(genFileURI())) {
          case (build, fileURIs) =>
            implicit val compiler: CompilerAccess =
              mock[CompilerAccess]

            // expect the compiler to get a request to fetch files
            // from the configured contract path.
            (compiler.getSourceFiles _)
              .expects(build.contractURI)
              .returns(Right(fileURIs)) // return files successfully fetched
              .once() // called only once

            // Initialise a workspace for the config
            val actualWorkspace =
              Workspace.initialise(build)

            // All files are started in OnDisk state.
            val expectedWorkspace =
              WorkspaceState.UnCompiled(
                build = build,
                sourceCode = fileURIs.map(SourceCodeState.OnDisk).to(ArraySeq)
              )

            actualWorkspace.value shouldBe expectedWorkspace
        }
      }
    }

    "on failure" should {
      "report an error" in {
        forAll(genWorkspaceBuild(), genError()) {
          case (build, error) =>
            implicit val compiler: CompilerAccess =
              mock[CompilerAccess]

            // expect the compiler to get a request to fetch files
            // from the configured contract path.
            (compiler.getSourceFiles _)
              .expects(build.contractURI)
              .returns(Left(error)) // return an error
              .once() // called only once

            // Initialise a workspace for the config
            val actualWorkspace =
              Workspace.initialise(build)

            // No initialisation occurs and a failure is returned.
            actualWorkspace.left.value shouldBe error
        }
      }
    }
  }

  "parse" when {
    "workspace is empty" should {
      "return existing state" in {
        forAll(GenWorkspace.genUnCompiled(Gen.const(List.empty))) {
          initialWorkspace =>
            implicit val compiler: CompilerAccess =
              null // compiler does not get accessed

            val actualWorkspace =
              Workspace.parse(initialWorkspace)

            // expect exising workspace
            actualWorkspace shouldBe initialWorkspace

        }
      }
    }

    /**
     * Test-cases handling [[org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState.OnDisk]] state.
     */
    "existing code is in OnDisk state" should {
      // generate a workspace with all files on-disk.
      val generator =
        GenWorkspace.genUnCompiled(Gen.nonEmptyListOf(GenSourceCode.genOnDisk()))

      "return failed state" when {
        "access to source-code fails" in {
          forAll(generator) {
            initialWorkspace =>
              implicit val compiler: CompilerAccess =
                mock[CompilerAccess]

              val expectedSourceCode =
                initialWorkspace.sourceCode map {
                  case currentState: SourceCodeState.OnDisk =>
                    // for every onDisk state return an errored state by the compiler
                    val expectedState =
                      SourceCodeState.ErrorAccess(
                        fileURI = currentState.fileURI,
                        error = genError().sample.get
                      )

                    // expect the compiler to get a request to read sourceCode for each OnDisk file once
                    // return an error for each call
                    (compiler.getSourceCode _)
                      .expects(currentState.fileURI)
                      .returns(Left(expectedState.error)) // return an error
                      .once() // called only once

                    expectedState
                }

              val actualWorkspace =
                Workspace.parse(initialWorkspace)

              // expect all error files
              val expectedWorkspace =
                WorkspaceState.UnCompiled(
                  build = initialWorkspace.build,
                  sourceCode = expectedSourceCode
                )

              actualWorkspace shouldBe expectedWorkspace
          }
        }
      }

      "return success state" when {
        "access to source-code passes" in {
          forAll(generator) {
            initialWorkspace =>
              implicit val compiler: CompilerAccess =
                mock[CompilerAccess]

              val expectedSourceCode =
                initialWorkspace.sourceCode map {
                  case currentState: SourceCodeState.OnDisk =>
                    // for every onDisk state return a successful parsed state
                    val expectedState =
                      SourceCodeState.Parsed(
                        fileURI = currentState.fileURI,
                        code = genCode.sample.get,
                        contracts = GenSourceCode.genParsedContracts().sample.get
                      )

                    // expect the compiler to get a request to read sourceCode and then parse
                    // for each OnDisk file once.
                    // this happens in order
                    inOrder(
                      // initially, code gets read from disk
                      (compiler.getSourceCode _)
                        .expects(currentState.fileURI)
                        .returns(Right(expectedState.code)) // code read!
                        .once(),

                      // then the read code gets parsed
                      (compiler.parseContracts _)
                        .expects(expectedState.code) // expect the read source code
                        .returns(Right(expectedState.contracts)) // code successfully parsed!
                        .once() // called only once
                    )

                    expectedState
                }

              val actualWorkspace =
                Workspace.parse(initialWorkspace)

              // expect all error files
              val expectedWorkspace =
                WorkspaceState.Parsed(
                  build = initialWorkspace.build,
                  sourceCode = expectedSourceCode
                )

              actualWorkspace shouldBe expectedWorkspace
          }
        }
      }
    }

    /**
     * Test-cases handling [[org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState.ParsedState]] state.
     */
    "good source code is already in Parsed state" should {
      "return a valid Parsed workspace state without accessing the compiler" in {
        val genGoodCode =
          GenWorkspace.genUnCompiled(Gen.nonEmptyListOf(GenSourceCode.genParsedOrCompiled()))

        forAll(genGoodCode) {
          initialWorkspace =>
            // compiler does not get accessed
            implicit val compiler: CompilerAccess =
              null

            val actualWorkspace =
              Workspace.parse(initialWorkspace)

            val expectedSourceCode =
              initialWorkspace.sourceCode collect {
                case parsed: SourceCodeState.Parsed     => parsed
                case compiled: SourceCodeState.Compiled => compiled.parsed
              }

            // return workspace state must be in Parsed state so it can be compiled.
            val expectedWorkspace =
              WorkspaceState.Parsed(
                build = initialWorkspace.build,
                sourceCode = expectedSourceCode
              )

            actualWorkspace shouldBe expectedWorkspace
        }
      }
    }

    "partially errored workspace" should {
      "always return UnCompiled state" in {
        // generate good workspace source code (i.e. either already parsed or compiled),
        // with at least one error source code.

        val generator =
          GenWorkspace.genParsedOrCompiledWithAtLeastOneFailed()

        forAll(generator) {
          initialWorkspace =>
            // compiler does not get accessed
            implicit val compiler: CompilerAccess =
              mock[CompilerAccess]

            val expectedSourceCode =
              initialWorkspace.sourceCode map {
                case error: SourceCodeState.ErrorAccess =>
                  // only the files that failed access would get a single re-try from the compiler.
                  (compiler.getSourceCode _)
                    .expects(error.fileURI)
                    .returns(Left(error.error))
                    .once()

                  error

                case other =>
                  // other code remains the same
                  other
              }

            val actualWorkspace =
              Workspace.parse(initialWorkspace)

            // expect the workspace to remain in UnCompiled state because after the parse, code still has an error code file.
            val expectedWorkspace =
              WorkspaceState.UnCompiled(
                build = initialWorkspace.build,
                sourceCode = expectedSourceCode
              )

            actualWorkspace shouldBe expectedWorkspace
        }
      }
    }
  }
}
