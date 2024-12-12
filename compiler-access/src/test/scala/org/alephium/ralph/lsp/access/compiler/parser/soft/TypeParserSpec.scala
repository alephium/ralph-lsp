package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypeParserSpec extends AnyWordSpec with Matchers {

  "type expected" when {
    "empty" in {
      parseType("") shouldBe SoftAST.IdentifierExpected(indexOf(">><<"))
    }
  }

  "type is provided" in {
    parseType("one") shouldBe
      Identifier(
        index = indexOf(">>one<<"),
        text = "one"
      )
  }

  "tuple 1" when {
    "closing parentheses is missing" in {
      val tpe = parseType("( one")

      tpe shouldBe a[SoftAST.Tuple]
    }

    "closing parentheses is provided" in {
      val tpe = parseType("( one)")

      tpe shouldBe a[SoftAST.Tuple]
    }
  }

  "tuple 2" when {
    "second type is missing" in {
      val tpe = parseType("(one, )")

      tpe shouldBe a[SoftAST.Tuple]
    }

    "second type is a simple type name" in {
      val tpe = parseType("(one, two)")

      tpe shouldBe a[SoftAST.Tuple]
    }

    "second type is another tuple" in {
      val tpe = parseType("(one, (two, three))")

      tpe shouldBe a[SoftAST.Tuple]
    }

  }

}
