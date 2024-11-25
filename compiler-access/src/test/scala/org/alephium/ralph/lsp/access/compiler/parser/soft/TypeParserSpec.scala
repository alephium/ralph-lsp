package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypeParserSpec extends AnyWordSpec with Matchers {

  "type expected" when {
    "empty" in {
      parseType("") shouldBe SoftAST.TypeExpected(indexOf(">><<"))
    }
  }

  "type is provided" in {
    parseType("one") shouldBe SoftAST.Type("one", indexOf(">>one<<"))
  }

  "tuple 1" when {
    "closing parentheses is missing" in {
      val tpe = parseType("( one")

      val expected =
        SoftAST.TupledType(
          index = indexOf(">>( one<<"),
          openParen = SoftAST.OpenParen(indexOf(">>(<< one")),
          preHeadTypeSpace = Some(SoftAST.Space(" ", indexOf("(>> <<one"))),
          headType = SoftAST.Type("one", indexOf("( >>one<<")),
          tailTypes = Seq.empty,
          closeParen = SoftAST.CloseParenExpected(indexOf("( one>><<"))
        )

      tpe shouldBe expected
    }

    "closing parentheses is provided" in {
      val tpe = parseType("( one)")

      val expected =
        SoftAST.TupledType(
          index = indexOf(">>( one)<<"),
          openParen = SoftAST.OpenParen(indexOf(">>(<< one)")),
          preHeadTypeSpace = Some(SoftAST.Space(" ", indexOf("(>> <<one)"))),
          headType = SoftAST.Type("one", indexOf("( >>one<<)")),
          tailTypes = Seq.empty,
          closeParen = SoftAST.CloseParen(indexOf("( one>>)<<"))
        )

      tpe shouldBe expected
    }
  }

  "tuple 2" when {
    "second type is missing" in {
      val tpe = parseType("(one, )").asInstanceOf[SoftAST.TupledType]

      tpe.tailTypes should have size 1
      val secondType = tpe.tailTypes.head

      val expected =
        SoftAST.TailType(
          index = indexOf("(one>>, <<)"),
          comma = SoftAST.Comma(indexOf("(one>>,<< )")),
          preTypeNameSpace = Some(SoftAST.Space(" ", indexOf("(one,>> <<)"))),
          tpe = SoftAST.TypeExpected(indexOf("(one, >><<)")),
          postTypeNameSpace = None
        )

      secondType shouldBe expected
    }

    "second type is a simple type name" in {
      val tpe = parseType("(one, two)").asInstanceOf[SoftAST.TupledType]

      tpe.tailTypes should have size 1
      val secondType = tpe.tailTypes.head

      val expected =
        SoftAST.TailType(
          index = indexOf("(one>>, two<<)"),
          comma = SoftAST.Comma(indexOf("(one>>,<< two)")),
          preTypeNameSpace = Some(SoftAST.Space(" ", indexOf("(one,>> <<two)"))),
          tpe = SoftAST.Type(
            code = "two",
            index = indexOf("(one, >>two<<)")
          ),
          postTypeNameSpace = None
        )

      secondType shouldBe expected
    }

    "second type is another tuple" in {
      val tpe = parseType("(one, (two, three))").asInstanceOf[SoftAST.TupledType]

      tpe.tailTypes should have size 1
      val tailType = tpe.tailTypes.head
      tailType.tpe shouldBe a[SoftAST.TupledType]
      tailType.tpe.toCode() shouldBe "(two, three)"
    }

  }

}
