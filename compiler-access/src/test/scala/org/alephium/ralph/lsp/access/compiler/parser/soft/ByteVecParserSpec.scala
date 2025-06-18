// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ByteVecParserSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  private def testHex(hex: String) = {
    val ast = parseByteVec(s"#$hex")

    val expected =
      SoftAST.ByteVec(
        index = indexOf(s">>#$hex<<"),
        hash = Hash(indexOf(s">>#<<$hex")),
        hex = Option.when(hex.nonEmpty)(CodeString(s"#>>$hex<<"))
      )

    ast shouldBe expected
  }

  "empty hex" in {
    testHex("")
  }

  "non-empty hex" when {
    "Int.Max" in {
      testHex(Int.MaxValue.toHexString)
    }

    "Int.Min" in {
      testHex(Int.MinValue.toHexString)
    }
  }

  "random hex" in {
    forAll(Gen.hexStr)(testHex)
  }

}
