package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.GenCommon._
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq

/**
 * Test cases for [[Workspace.compile]] function.
 */
class WorkspaceCompileSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks with MockFactory {

  "return error state" when {
    "compilation fails" in {
      forAll(GenWorkspace.genParsed(), genError()) {
        case (initialWorkspace, compilationError) =>
          implicit val compiler: CompilerAccess =
            mock[CompilerAccess]

          // collect all contracts for this compilation
          val contracts =
            initialWorkspace.sourceCode.flatMap(_.contracts)

          // the compilation returned an error
          (compiler.compileContracts _)
            .expects(contracts, initialWorkspace.build.config.compilerOptions)
            .returns(Left(compilationError))

          val actualWorkspace =
            Workspace.compile(initialWorkspace)

          // expect the state to carry previous valid state's information and the compilation error.
          val expectedWorkspace =
            WorkspaceState.Errored(
              sourceCode = initialWorkspace.sourceCode,
              workspaceErrors = ArraySeq(compilationError),
              parsed = initialWorkspace
            )

          actualWorkspace shouldBe expectedWorkspace
      }
    }
  }
}
