// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TestDeclarationParserSpec extends AnyWordSpec with Matchers {

  "parse a test name and body with exact keyword range" in {
    val test = parseTestDeclaration("test \"example\" { let value = 1 }")
    test.keyword.code.token shouldBe Token.Test
    test.keyword.index shouldBe indexOf(">>test<< \"example\" { let value = 1 }")
    test.name.toCode() shouldBe "\"example\""
    test
      .parts
      .collect {
        case block: SoftAST.Block => block
      }
      .size shouldBe 1
  }

  "parse settings, constructor aliases, approval and repeated test bodies" in {
    val code = """test "example"
      |with updateImmFields = true, updateMutFields = false
      |before Foo(1)@first, Foo(2)@second
      |after Foo(3)
      |approve { first -> ALPH: 1 alph }
      |{ let value = 1 }
      |before Foo(4) { let value = 2 }
      |""".stripMargin.trim
    val test = parseTestDeclaration(code)
    test.parts.collect {
      case clause: SoftAST.TestClause => clause.keyword.code.token
    } shouldBe
      Seq(Token.With, Token.Before, Token.After, Token.Approve, Token.Before)
    test
      .parts
      .collect {
        case block: SoftAST.Block => block
      }
      .size shouldBe 2
    test.toNode.walk.exists(_.data.isInstanceOf[SoftAST.Unresolved]) shouldBe false
    test.toNode.walk.collect { case node if node.data.isInstanceOf[SoftAST.Identifier] => node.data.toCode() }.toSeq should
      contain noneOf ("test", "before", "after", "approve", "with")
  }

  "retain incomplete test declarations while editing" in {
    Seq("test \"unfinished", "test \"name\"", "test \"name\" before Foo(", "test \"name\" { let value =").foreach {
      code =>
        parseTestDeclaration(code).toCode() shouldBe code
    }
  }

  "keep adjacent test and function declarations separate" in {
    val root = parseSoft("test \"first\" {} test \"second\" {} fn next() {}")
    root
      .parts
      .collect {
        case test: SoftAST.TestDeclaration => test
      }
      .size shouldBe 2
    root
      .parts
      .collect {
        case function: SoftAST.Function => function
      }
      .size shouldBe 1
  }

  "preserve comments and distinguish calls or assignments after a test" in {
    parseTestDeclaration("// test docs\ntest \"commented\" // setup\nbefore Foo() { // body\n}")
    val root = parseSoft("test \"first\" {} before = 1 test(2)")
    root.toNode.walk.count(_.data.isInstanceOf[SoftAST.TestDeclaration]) shouldBe 1
    root.toNode.walk.exists(_.data.isInstanceOf[SoftAST.Assignment]) shouldBe true
    root.toNode.walk.exists(_.data.isInstanceOf[SoftAST.ReferenceCall]) shouldBe true
  }

  "keep contextual keywords available as ordinary identifiers" in {
    Seq("test", "before", "after", "with", "approve", "testing").foreach {
      name =>
        parseIdentifier(name).toCode() shouldBe name
        val root = parseSoft(s"let $name = 1")
        root.toNode.walk.exists(_.data.isInstanceOf[SoftAST.TestDeclaration]) shouldBe false
    }
  }

}
