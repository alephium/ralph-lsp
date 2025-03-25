// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.util

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.access.compiler.message._
import org.alephium.ralph.lsp.access.util.StringUtil._

class StringUtilSpec extends AnyWordSpec with Matchers {

  def computeIndex(code: String): Assertion = {
    val (linePosition, index, codeWithoutSymbol) = TestCodeUtil.indicatorPositionOrFail(code)
    StringUtil.computeIndex(codeWithoutSymbol, linePosition.line, linePosition.character) shouldBe index
  }

  "StringUtil" should {
    "compute index" in {
      computeIndex("line1\n@@line2")
      computeIndex("\n@@line1\nline2")
      computeIndex("\r@@line1\rline2")
      computeIndex("line1\r@@line2")
      computeIndex("line1\rli@@ne2")
      computeIndex("line1\nline2\nline3\nlin@@e4")
      computeIndex("line1\rline2\rline3\rline4@@")
      computeIndex("line1\r\nline2\r\nline3\r\nline4@@")
      computeIndex("line1\nline2\rline3\r\nline4@@")
      computeIndex("line1\nline2\rline3\r\n@@line4")
      computeIndex("line1\n\r\r\n\r@@line2")
    }
  }

  def testLineRange(code: String): Assertion = {

    val (expectedLineRanges, codeWithoutSymbol, from, to) = TestCodeUtil.lineRanges(code)

    buildLineRange(codeWithoutSymbol, from, to) shouldBe expectedLineRanges.head
  }

  "SourceIndexExtra" should {
    "build line range" in {
      testLineRange(">>l<<ine1\nline2\r\nline3\rline4\n")
      testLineRange(">>line1<<\nline2\r\nline3\rline4\n")
      testLineRange("line1\nli>>n<<e2\r\nline3\rline4\n")
      testLineRange("line1\nline2\r\nline3\r>>line4<<\n")
      testLineRange(">>line1\nline2\r\nli<<ne3\rline4\n")
      testLineRange(">>line1\nline2\r\nline3\rline4<<\n")
      testLineRange("line1\n>>line2\r\nline3\rline4<<\n")
      testLineRange("line1\nline2\r\n>>line3\rline4<<\n")
    }

    "fail to build time range with invalid arguments" in {
      val code = "line1\nline2\r\nline3\rline4"
      buildLineRange(code, 1, 0) shouldBe LineRange.zero
      buildLineRange(code, -1, 1) shouldBe LineRange.zero
      buildLineRange(code, 1, -1) shouldBe LineRange.zero
      buildLineRange(code, code.length, code.length) shouldBe LineRange.zero
      buildLineRange(code, code.length + 1, code.length + 1) shouldBe LineRange.zero
    }

    "end-of-file" should {
      "succeed" when {
        "range spans the entire file" in {
          testLineRange(">>line1\nline2\r\nline3\rline4<<")
        }

        "range spans only the last character" in {
          testLineRange("line1\nline2\r\nline3\rline>>4<<")
        }
      }
    }
  }

}
