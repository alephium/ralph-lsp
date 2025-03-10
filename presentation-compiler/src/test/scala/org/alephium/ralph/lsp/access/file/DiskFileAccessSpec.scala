// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.file

import org.alephium.ralph.lsp.{TestFile, TestCode}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class DiskFileAccessSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "readIfExists" should {
    "return None" when {
      "file does not exist" in {
        forAll(TestFile.genFileURI()) {
          fileURI =>
            DiskFileAccess.readIfExists(fileURI) shouldBe Right(None)
        }
      }
    }

    "return file content" when {
      "file exists" in {
        forAll(TestFile.genFileURI(), TestCode.genGoodOrBad()) {
          case (fileURI, code) =>
            TestFile.write(fileURI, code)
            DiskFileAccess.readIfExists(fileURI) shouldBe Right(Some(code))
            TestFile delete fileURI
            DiskFileAccess.readIfExists(fileURI) shouldBe Right(None)
        }
      }
    }
  }

}
