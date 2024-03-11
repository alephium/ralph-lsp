package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.error.StringError
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState, TestBuild}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq

/**
 * Test cases for [[Workspace.initialise]] function.
 */
class WorkspaceInitialiseSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks with MockFactory {

  implicit val clientLogger: ClientLogger =
    TestClientLogger

  implicit val compiler: CompilerAccess =
    CompilerAccess.ralphc

  "fail" when {
    "input build state is already an error state" in {
      // generator that emits error build files
      val generator =
        TestBuild
          .genParsed()
          .map {
            parsed =>
              val errored =
                Build.compile(
                  parsed = parsed,
                  currentBuild = None
                )(FileAccess.disk, compiler, clientLogger)

              // errored because contractsURI is not a persisted folder
              errored.asInstanceOf[BuildState.BuildErrored]
          }

      forAll(generator) {
        expectedError =>
          // no IO should occur
          implicit val file: FileAccess =
            null

          val actualError =
            Workspace.initialise(expectedError: BuildState.IsCompiled)

          // the same state is returned
          actualError.left.value shouldBe expectedError
      }
    }

    "there is an IO error" in {
      // initially there exists a valid build file.
      val generator =
        TestBuild
          .genCompiledOK()(FileAccess.disk, compiler, clientLogger)
          .map {
            compiled =>
              // randomly set dependency to None or Some
              val dependencyRandom =
                Gen
                  .oneOf(None, compiled.dependency)
                  .sample
                  .get

              compiled.copy(dependency = dependencyRandom)
          }

      forAll(generator) {
        initialBuild =>
          // use mock to inject IO error
          implicit val file: FileAccess =
            mock[FileAccess]

          val errorIO =
            StringError("Kaboom!", initialBuild.buildURI)

          // inject IO error
          (file.list _)
            .expects(initialBuild.contractURI)
            .returns(Left(errorIO))
            .once()

          val actualError =
            Workspace.initialise(initialBuild: BuildState.IsCompiled)

          val expectedError =
            BuildState.BuildErrored(
              buildURI = initialBuild.buildURI,
              code = Some(initialBuild.code),
              errors = ArraySeq(errorIO), // the error is reported
              dependency = initialBuild.dependency, // dependency is carried forward
              activateWorkspace = None // continue with existing workspace
            )

          actualError.left.value shouldBe expectedError
      }
    }
  }

  "succeed" when {
    "input build is compiled OK" in {
      implicit val fileAccess: FileAccess =
        FileAccess.disk

      // the input build is good!
      forAll(TestBuild.genCompiledWithSourceCode()) {
        case (buildCompiled, sourceCode) =>
          val actualWorkspace =
            Workspace
              .initialise(buildCompiled: BuildState.IsCompiled)
              .value

          val actualWorkspaceSorted =
            actualWorkspace.copy(sourceCode = actualWorkspace.sourceCode.sortBy(_.fileURI))

          val expectedWorkspace =
            WorkspaceState.UnCompiled(
              build = buildCompiled,
              sourceCode = sourceCode.sortBy(_.fileURI).to(ArraySeq)
            )

          actualWorkspaceSorted shouldBe expectedWorkspace
      }
    }
  }
}
