package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.config.GenCommon._
import org.alephium.ralph.lsp.pc.sourcecode.{GenSourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.GenWorkspace._
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
        forAll(genWorkspaceConfig(), Gen.listOf(genFileURI())) {
          case (config, fileURIs) =>
            implicit val compiler: CompilerAccess =
              mock[CompilerAccess]

            // expect the compiler to get a request to fetch files
            // from the configured contract path.
            (compiler.getSourceFiles _)
              .expects(config.contractURI)
              .returns(Right(fileURIs)) // return files successfully fetched
              .once() // called only once

            // Initialise a workspace for the config
            val actualWorkspace =
              Workspace.initialise(config)

            // All files are started in OnDisk state.
            val expectedWorkspace =
              WorkspaceState.UnCompiled(
                config = config,
                sourceCode = fileURIs.map(SourceCodeState.OnDisk).to(ArraySeq)
              )

            actualWorkspace.value shouldBe expectedWorkspace
        }
      }
    }

    "on failure" should {
      "report an error" in {
        forAll(genWorkspaceConfig(), genError()) {
          case (config, error) =>
            implicit val compiler: CompilerAccess =
              mock[CompilerAccess]

            // expect the compiler to get a request to fetch files
            // from the configured contract path.
            (compiler.getSourceFiles _)
              .expects(config.contractURI)
              .returns(Left(error)) // return an error
              .once() // called only once

            // Initialise a workspace for the config
            val actualWorkspace =
              Workspace.initialise(config)

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
      // generate a workspace will all files on-disk.
      val generator =
        GenWorkspace.genUnCompiled(Gen.nonEmptyListOf(GenSourceCode.genOnDisk()))

      "return failed state" when {
        "access to source-code fails" in {
          forAll(generator) {
            initialWorkspace =>
              implicit val compiler: CompilerAccess =
                mock[CompilerAccess]

              // currentState and expectedState
              val testData =
                initialWorkspace.sourceCode map {
                  case currentState: SourceCodeState.OnDisk =>
                    // for every onDisk state return an errored state by the compiler
                    val expectedState =
                      SourceCodeState.ErrorAccess(
                        fileURI = currentState.fileURI,
                        error = genError().sample.get
                      )

                    (currentState, expectedState)
                }

              // expect the compiler to get a request to read sourceCode for each OnDisk file once
              // return an error for each call
              testData foreach {
                case (currentState, expectedState) =>
                  (compiler.getSourceCode _)
                    .expects(currentState.fileURI)
                    .returns(Left(expectedState.error)) // return an error
                    .once() // called only once
              }

              val actualWorkspace =
                Workspace.parse(initialWorkspace)

              // expect all error files
              val expectedWorkspace =
                WorkspaceState.UnCompiled(
                  config = initialWorkspace.config,
                  sourceCode = testData.map(_._2)
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

              // currentState and expectedState
              val testData =
                initialWorkspace.sourceCode map {
                  case currentState: SourceCodeState.OnDisk =>
                    // for every onDisk state return a successful parsed state
                    val expectedState =
                      SourceCodeState.Parsed(
                        fileURI = currentState.fileURI,
                        code = genCode.sample.get,
                        contracts = GenSourceCode.genParsedContracts().sample.get
                      )

                    (currentState, expectedState)
                }

              // expect the compiler to get a request to read sourceCode and then parse
              // for each OnDisk file once.
              testData foreach {
                case (currentState, expectedState) =>
                  // this happens in order
                  inOrder(
                    // code gets read from disk
                    (compiler.getSourceCode _)
                      .expects(currentState.fileURI)
                      .returns(Right(expectedState.code)) // code read!
                      .once(),

                    // the read code gets parsed
                    (compiler.parseContracts _)
                      .expects(expectedState.code) // expect the read source code
                      .returns(Right(expectedState.contracts)) // code successfully parsed!
                      .once() // called only once
                  )
              }

              val actualWorkspace =
                Workspace.parse(initialWorkspace)

              // expect all error files
              val expectedWorkspace =
                WorkspaceState.Parsed(
                  config = initialWorkspace.config,
                  sourceCode = testData.map(_._2)
                )

              actualWorkspace shouldBe expectedWorkspace
          }
        }
      }
    }
  }
}
