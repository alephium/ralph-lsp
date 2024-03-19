package org.alephium.ralph.lsp.access.util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.access.compiler.message._
import org.alephium.ralph.lsp.access.util.StringUtil.computeIndex


class StringUtilSpec extends AnyWordSpec with Matchers {

  def computeIndex(code: String): Unit = {
    val index = code.indexOf("@")
    val lines = code.split("\n|\r\n|\r").zipWithIndex
    val (line, lineIndex) = lines.find(_._1.contains("@")).get
    val col = line.indexOf("@")
    StringUtil.computeIndex(code.replace("@",""), lineIndex, col) shouldBe index
  }

  "StringUtil" should {
    "compute index" in {
        computeIndex("line1\n@line2")
        computeIndex("\n@line1\nline2")
        computeIndex("\r@line1\rline2")
        computeIndex("line1\r@line2")
        computeIndex("line1\rli@ne2")
        computeIndex("line1\nline2\nline3\nlin@e4")
        computeIndex("line1\rline2\rline3\rline4@")
        computeIndex("line1\r\nline2\r\nline3\r\nline4@")
        computeIndex("line1\nline2\rline3\r\nline4@")
        computeIndex("line1\nline2\rline3\r\n@line4")
        computeIndex("line1\n\r\r\n\r@line2")
    }
  }
}
