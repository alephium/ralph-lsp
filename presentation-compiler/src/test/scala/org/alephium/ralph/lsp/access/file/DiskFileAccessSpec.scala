// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

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
