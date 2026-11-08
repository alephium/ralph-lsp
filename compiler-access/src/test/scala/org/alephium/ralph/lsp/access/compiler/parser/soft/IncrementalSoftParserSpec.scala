// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse.Parsed
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IncrementalSoftParserSpec extends AnyWordSpec with Matchers {

  private val first    = "Contract First() { pub fn value() -> U256 { return 1 } }"
  private val second   = "Contract Second() { pub fn value() -> U256 { return 2 } }"
  private val original = first + "\n" + second

  private def full(code: String): SoftAST.RootBlock = fastparse.parse(code, SoftParser.parse(_)).get.value

  private def compare(
      oldCode: String,
      nextCode: String): org.scalatest.Assertion = {
    val old      = full(oldCode)
    val actual   = IncrementalSoftParser.parse(nextCode, oldCode, old)
    val expected = fastparse.parse(nextCode, SoftParser.parse(_))
    (actual, expected) match {
      case (Parsed.Success(tree, index), Parsed.Success(fullTree, fullIndex)) =>
        tree shouldBe fullTree
        index shouldBe fullIndex
        tree.toCode() shouldBe nextCode
        tree.index.width shouldBe nextCode.length
      case (failure: Parsed.Failure, fullFailure: Parsed.Failure) =>
        failure.index shouldBe fullFailure.index
        failure.trace().longMsg shouldBe fullFailure.trace().longMsg
      case _ => fail(s"Incremental parse differs from full parse: $actual / $expected")
    }
  }

  "incremental soft parsing" should {
    "reuse a complete prefix template when editing a later template" in {
      val old                    = full(original)
      val updated                = original.replace("return 2", "return 200")
      val (_, nodes, characters) = IncrementalSoftParser.parseWithStats(updated, original, old)
      nodes should be > 0
      characters shouldBe first.length
      info(s"Top-level edit reused $nodes declarations and $characters UTF-16 code units")
      compare(original, updated)
    }
    "reuse an earlier function inside the same edited contract" in {
      val oldCode                = "Contract Only() { pub fn first() -> U256 { return 1 } pub fn second() -> U256 { return 2 } }"
      val updated                = oldCode.replace("return 2", "return 222")
      val (_, nodes, characters) = IncrementalSoftParser.parseWithStats(updated, oldCode, full(oldCode))
      nodes should be > 0
      characters should be > 0
      characters should be < oldCode.length
      info(s"Same-contract edit reused $nodes declarations and $characters UTF-16 code units")
      compare(oldCode, updated)
    }
    "keep old and new Node parent chains independent" in {
      val old         = full(original)
      val oldRoot     = old.toNode
      val oldChildren = oldRoot.children
      val next        = IncrementalSoftParser.parse(original.replace("return 2", "return 3"), original, old).get.value
      val nextRoot    = next.toNode
      oldChildren.foreach(
        child => (child.parent.get eq oldRoot) shouldBe true
      )
      nextRoot
        .children
        .foreach(
          child => (child.parent.get eq nextRoot) shouldBe true
        )
      next.parts.head shouldBe old.parts.head
      (next.parts.head eq old.parts.head) shouldBe false
    }
    "match full parsing across insertions, removals and malformed edits" in {
      val insertions = Seq("x", "}", "{", "\"", "//", "\n", "\r\n", "😀", "/*")
      val positions  = Seq(0, 9, first.length, first.length + 1, original.indexOf("return 2"), original.length)
      for {
        position <- positions
        text     <- insertions
      } {
        compare(original, original.patch(position, text, 0))
        if (position < original.length) compare(original, original.patch(position, text, 1))
      }
      compare(original, first)
      compare(original, "")
      compare(original, original)
    }
    "fall back for uncertain prefixes and edits to the first template" in {
      val malformed = "Contract First() { pub fn value() -> U256 { return \"unterminated"
      val updated   = malformed + "\" } }"
      IncrementalSoftParser.parseWithStats(updated, malformed, full(malformed))._2 shouldBe 0
      compare(malformed, updated)
      val changed = original.replace("First", "Renamed")
      IncrementalSoftParser.parseWithStats(changed, original, full(original))._2 shouldBe 0
      compare(original, changed)
    }
    "preserve absolute offsets through repeated edits with Unicode and CRLF" in {
      var previous = original.replace("\n", "\r\n")
      Seq("return 22", "return 222", "return 2", "return \"😀\"").foreach {
        replacement =>
          val start = previous.lastIndexOf("return ")
          val end   = previous.indexOf(" }", start)
          val next  = previous.substring(0, start) + replacement + previous.substring(end)
          compare(previous, next)
          previous = next
      }
    }
  }

}
