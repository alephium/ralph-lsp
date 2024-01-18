package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.message.error.TestError
import org.alephium.ralph.lsp.access.file.FileAccess
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ArraySeq

class SourceCodePutOrRemoveSpec extends AnyWordSpec with Matchers with MockFactory with ScalaCheckDrivenPropertyChecks {

  "putOrRemove" should {
    "insert new source-code" when {
      "updated source-code is provided" in {
        // since the source-code is provided, disk access should NOT occur, so set FileAccess to null.
        implicit val file: FileAccess =
          null

        val (onDisk, code) =
          TestSourceCode.genOnDiskAndPersist().sample.get

        val newSourceCode =
          SourceCode.putOrRemove(
            fileURI = onDisk.fileURI,
            updatedCode = Some(code),
            sourceCode = ArraySeq(onDisk)
          )

        newSourceCode should contain only
          SourceCodeState.UnCompiled(
            fileURI = onDisk.fileURI,
            code = code
          )

        TestSourceCode delete onDisk
      }

      "updated source-code is not provided" in {
        implicit val file: FileAccess =
          FileAccess.disk

        val (onDisk, _) =
          TestSourceCode.genOnDiskAndPersist().sample.get

        val newSourceCode =
          SourceCode.putOrRemove(
            fileURI = onDisk.fileURI,
            updatedCode = None,
            sourceCode = ArraySeq(onDisk)
          )

        newSourceCode should contain only SourceCodeState.OnDisk(onDisk.fileURI)

        TestSourceCode delete onDisk
      }
    }

    "remove source-code" when {
      "fileURI does not exists onDisk" in {
        implicit val file: FileAccess =
          FileAccess.disk

        // This source-code is not persisted.
        val inMemory =
          TestSourceCode.genOnDisk().sample.get

        val newSourceCode =
          SourceCode.putOrRemove(
            fileURI = inMemory.fileURI,
            updatedCode = None,
            sourceCode = ArraySeq(inMemory)
          )

        // expect it to be removed
        newSourceCode shouldBe empty
      }
    }

    "add error source-code state" when {
      "access to disk fails" in {
        // create an onDisk start
        val (onDisk, code) =
          TestSourceCode.genOnDiskAndPersist().sample.get

        // the IO error to inject
        val ioError =
          TestError.genError(code).sample.get

        // mock to inject IO error
        implicit val file: FileAccess =
          mock[FileAccess]

        // return an error accessing disk
        (file.exists _)
          .expects(onDisk.fileURI)
          .returns(Left(ioError))

        // insert
        val newSourceCode =
          SourceCode.putOrRemove(
            fileURI = onDisk.fileURI,
            updatedCode = None,
            sourceCode = ArraySeq(onDisk)
          )

        // expect the fileURI to report the error
        newSourceCode should contain only
          SourceCodeState.ErrorAccess(
            fileURI = onDisk.fileURI,
            error = ioError
          )

        TestSourceCode delete onDisk
      }
    }
  }
}
