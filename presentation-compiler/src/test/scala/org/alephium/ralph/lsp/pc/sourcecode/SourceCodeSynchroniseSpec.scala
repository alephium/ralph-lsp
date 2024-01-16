package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.GenWorkspace
import org.alephium.ralph.lsp.FileIO
import org.alephium.ralph.lsp.GenExtensions.GenExtensionsImplicits
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq
import scala.util.Random

class SourceCodeSynchroniseSpec extends AnyWordSpec with Matchers with MockFactory with ScalaCheckDrivenPropertyChecks {

  "synchronise" should {
    "always return source-code within the workspace" in {
      val generator =
        for {
          // a workspace with source-code inside the workspace
          (workspace, sourceCodeInside) <- GenWorkspace.genCreatedWithSourceCode(persist = true)
          // source code outside the workspace
          sourceCodeOutside <- Gen.listOfMax()(GenSourceCode.genOnDisk()).map(GenSourceCode.persistAll(_))
          // some or all of inside source-code
          someCodeInside <- Gen.someOf(sourceCodeInside)
          // some or all outside source-code
          someCodeOutside <- Gen.someOf(sourceCodeOutside)
        } yield
          (workspace, sourceCodeInside, sourceCodeOutside, someCodeInside, someCodeOutside)

      implicit val fileAccess: FileAccess =
        FileAccess.disk

      forAll(generator) {
        case (workspace, sourceCodeInside, sourceCodeOutside, someOfInside, someOfOutside) =>
          // random source-code i.e. some of inside and some of outside.
          val inputSourceCode =
            Random.shuffle(someOfInside ++ someOfOutside).to(ArraySeq)

          val result =
            SourceCode.synchronise(
              sourceDirectory = workspace.workspaceURI,
              sourceCode = inputSourceCode
            )

          // end result should ALWAYS contains all the source-code within the workspace
          result.value should contain theSameElementsAs sourceCodeInside

          // clear all generate files
          FileIO.deleteAll(workspace.workspaceURI)
          sourceCodeOutside.map(_.fileURI) foreach FileIO.delete
      }
    }
  }
}
