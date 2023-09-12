package org.alephium.ralph.lsp.pc.workspace

import org.alephium.protocol.vm.MutBalancesPerLockup.error
import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.compiler.error.FileError
import org.alephium.ralph.lsp.pc.config.GenCommon._
import org.alephium.ralph.lsp.pc.sourcecode.GenSourceCode._
import org.alephium.ralph.lsp.pc.sourcecode.{GenSourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.GenWorkspace._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq
import scala.util.Random

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

  //  "parse" should {
  //    "return un-compiled state" when {
  //      "one source-code compilation fails" in {
  //        forAll(GenWorkspace.genUnCompiled()) {
  //          initialWorkspace =>
  //
  //            val errorAccessingCode =
  //              initialWorkspace.sourceCode.collect {
  //                case code: SourceCodeState.ErrorAccess =>
  //                  (code, genError().sample.get)
  //              }
  //
  //            val erroredCode =
  //              initialWorkspace.sourceCode.collect {
  //                case code: SourceCodeState.Errored =>
  //                  (code, genError().sample.get)
  //              }
  //
  //            implicit val compiler: CompilerAccess =
  //              mock[CompilerAccess]
  //
  //            // expect the compiler to get a request to fetch files
  //            // from the configured contract path.
  //
  //            errorAccessingCode foreach {
  //              case (erroredCode, errorMessage) =>
  //                (compiler.getSourceCode _)
  //                  .expects(erroredCode.fileURI)
  //                  .returns(Left(errorMessage)) // return an error
  //                  .once() // called only once
  //            }
  //
  //            erroredCode foreach {
  //              cachedState =>
  //                (compiler.parseContracts _)
  //                  .expects(cachedState.code)
  //                  .returns(Right(Seq.empty)) // return an error
  //                  .once() // called only once
  //            }
  //
  //            val expectedSourceCode =
  //              initialWorkspace.sourceCode map {
  //                case badCode: SourceCodeState.ErrorState =>
  //                  val fileError = erroredStates.find(_._1 == badCode).value._2
  //                  badCode.updateError(fileError)
  //
  //                case goodCode: SourceCodeState.UnCompiled =>
  //                  SourceCodeState.Parsed
  //                  goodCode
  //              }
  //
  //            val expectedWorkspace =
  //              WorkspaceState.UnCompiled(
  //                config = initialWorkspace.config,
  //                sourceCode = expectedSourceCode
  //              )
  //
  //            val actualWorkspace =
  //              Workspace.parse(initialWorkspace)
  //
  //            actualWorkspace shouldBe expectedWorkspace
  //        }
  //      }
  //    }
  //  }

}
