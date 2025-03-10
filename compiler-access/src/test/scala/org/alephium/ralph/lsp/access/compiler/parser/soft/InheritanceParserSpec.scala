// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.access.util.TestFastParse._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InheritanceParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "implements is prefix of an identifier" in {
      assertIsFastParseError {
        parseInheritance("implementsABC")
      }
    }

    "extends is prefix of an identifier" in {
      assertIsFastParseError {
        parseInheritance("extendsABC")
      }
    }
  }

  "succeed" when {
    "implements & extends tokens are used without references or identifiers" when {
      def doTest(token: Token.Inheritance) = {
        val inheritance =
          parseInheritance(token.lexeme)

        inheritance shouldBe
          SoftAST.Inheritance(
            index = indexOf(s">>${token.lexeme}<<"),
            inheritanceType = TokenDocumented(indexOf(s">>${token.lexeme}<<"), token),
            postInheritanceTypeSpace = None,
            headReference = IdentifierExpected(s"${token.lexeme}>><<"),
            tailReferencesOrSpace = None
          )
      }

      "extends" in {
        doTest(Token.Extends)
      }

      "implements" in {
        doTest(Token.Implements)
      }
    }

    "there are multiple references" when {
      def doTest(token: Token.Inheritance) = {
        val inheritance =
          parseInheritance(s"${token.lexeme} A(), B")

        inheritance shouldBe
          SoftAST.Inheritance(
            index = indexOf(s">>${token.lexeme} A(), B<<"),
            inheritanceType = TokenDocumented(indexOf(s">>${token.lexeme}<< A(), B"), token),
            postInheritanceTypeSpace = Some(Space(s"${token.lexeme}>> <<A(), B")),
            headReference = SoftAST.ReferenceCall(
              index = indexOf(s"${token.lexeme} >>A()<<, B"),
              reference = Identifier(s"${token.lexeme} >>A<<(), B"),
              preArgumentsSpace = None,
              arguments = SoftAST.Group(
                index = indexOf(s"${token.lexeme} A>>()<<, B"),
                openToken = Some(OpenParen(s"${token.lexeme} A>>(<<), B")),
                preHeadExpressionSpace = None,
                headExpression = None,
                postHeadExpressionSpace = None,
                tailExpressions = Seq.empty,
                closeToken = Some(CloseParen(s"${token.lexeme} A(>>)<<, B"))
              )
            ),
            tailReferencesOrSpace = Some(
              Right(
                Seq(
                  SoftAST.TailReferences(
                    index = indexOf(s"${token.lexeme} A()>>, B<<"),
                    comma = Comma(s"${token.lexeme} A()>>,<< B"),
                    postCommaSpace = Some(Space(s"${token.lexeme} A(),>> <<B")),
                    reference = Identifier(s"${token.lexeme} A(), >>B<<"),
                    postReferenceSpace = None
                  )
                )
              )
            )
          )

      }

      "extends" in {
        doTest(Token.Extends)
      }

      "implements" in {
        doTest(Token.Implements)
      }

    }
  }

}
