package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.GenCommon._
import org.alephium.ralph.lsp.access.compiler.message.error.StringError
import org.alephium.ralph.lsp.access.file.FileAccess
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SourceCodeInitialiseSpec extends AnyWordSpec with Matchers with MockFactory with ScalaCheckDrivenPropertyChecks {

  "initialise" should {
    "report failure to fetch source files" in {
      forAll(genFolderURI()) {
        workspaceURI =>
          implicit val file: FileAccess =
            mock[FileAccess]

          val error =
            StringError("Something went wrong")

          (file.list _)
            .expects(workspaceURI)
            .returns(Left(error))

          val actualState =
            SourceCode.initialise(workspaceURI).left.value

          actualState shouldBe error

      }
    }
  }
}
