package org.alephium.ralph.lsp.access.util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class StringUtilSpec extends AnyWordSpec with Matchers {

  "StringUtil" should {
    "compute index" in {
      {
        val code  = "line1\nline2"
        StringUtil.previousComputeIndex(code, 1, 0) shouldBe 6
        StringUtil.computeIndex(code, 1, 0) shouldBe 6
      }
      {
        val code  = "line1\rline2"
        StringUtil.computeIndex(code, 1, 0) shouldBe 6
        StringUtil.previousComputeIndex(code, 1, 0) shouldBe 6
      }
      {
        val code  = "line1\nline2\nline3\nline4"
        StringUtil.previousComputeIndex(code, 3, 4) shouldBe 22
        StringUtil.computeIndex(code, 3, 4) shouldBe 22
      }
      {
        val code  = "line1\rline2\rline3\rline4"
        StringUtil.previousComputeIndex(code, 3, 4) shouldBe 22
        StringUtil.computeIndex(code, 3, 4) shouldBe 22
      }
      {
        val code  = "line1\r\nline2\r\nline3\r\nline4"
        StringUtil.previousComputeIndex(code, 3, 4) shouldBe 25
        StringUtil.computeIndex(code, 3, 4) shouldBe 25
      }
      {
        val code  = "line1\nline2\rline3\r\nline4"
        StringUtil.previousComputeIndex(code, 3, 4) shouldBe 23
        StringUtil.computeIndex(code, 3, 4) shouldBe 23
      }
    }

    "extract lines with separator length" in {
      {
        val code  = "line1\nline2\r\nline3\rline4"
        val expected = Array(("line1",1), ("line2",2), ("line3",1), ("line4",0))
        StringUtil.codeLines(code) shouldBe expected
      }
      {
        val code  = "line1\nline2\n"
        val expected = Array(("line1",1), ("line2",1), ("",0))
        StringUtil.codeLines(code) shouldBe expected
      }
      {
        val code  = "line1\rline2\r"
        val expected = Array(("line1",1), ("line2",1), ("",0))
        StringUtil.codeLines(code) shouldBe expected
      }
      {
        val code  = "line1\r\rline2"
        val expected = Array(("line1",1), ("",1), ("line2",0))
        StringUtil.codeLines(code) shouldBe expected
      }
      {
        val code  = "line1\n\rline2"
        val expected = Array(("line1",1), ("",1), ("line2",0))
        StringUtil.codeLines(code) shouldBe expected
      }
    }
  }
}
