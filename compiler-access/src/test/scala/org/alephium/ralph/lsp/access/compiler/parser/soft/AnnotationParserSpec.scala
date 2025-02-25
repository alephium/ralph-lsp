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

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.utils.Node
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class AnnotationParserSpec extends AnyWordSpec with Matchers {

  "error cases" should {
    "report missing identifier" in {
      val annotation =
        parseAnnotation("@")

      annotation shouldBe
        SoftAST.Annotation(
          index = indexOf(">>@<<"),
          at = At(">>@<<"),
          preIdentifierSpace = None,
          identifier = IdentifierExpected("@>><<"),
          postIdentifierSpace = None,
          tuple = None,
          postTupleSpace = None
        )
    }

    "report missing closing parenthesis" in {
      val annotation =
        parseAnnotation("@anno(")

      // opening paren is parsed
      annotation.tuple.value.openToken.value shouldBe OpenParen("@anno>>(<<")
      // closing paren is reported as expected
      annotation.tuple.value.closeToken.value shouldBe SoftAST.TokenExpected(indexOf("@anno(>><<"), Token.CloseParen)
    }

    "reject reserved keyword as annotation identifier" in {
      val root =
        // `fn` is a reserved keyword and cannot be used as an annotation identifier.
        // Similarly, `Contract TxScript etc` also cannot be used as an identifier
        parseSoft("@fn function()")

      val annotation =
        root
          .toNode
          .walkDown
          .collectFirst {
            case Node(annotation: SoftAST.Annotation, _) =>
              annotation
          }
          .value

      annotation shouldBe
        SoftAST.Annotation(
          index = indexOf(">>@<<fn function()"),
          at = At(">>@<<fn function()"),
          preIdentifierSpace = None,
          identifier = IdentifierExpected("@>><<fn function()"),
          postIdentifierSpace = None,
          tuple = None,
          postTupleSpace = None
        )
    }
  }

  "parse annotation identifier" in {
    val annotation =
      parseAnnotation("@anno")

    annotation shouldBe
      SoftAST.Annotation(
        index = indexOf(">>@anno<<"),
        at = At(">>@<<anno"),
        preIdentifierSpace = None,
        identifier = Identifier("@>>anno<<"),
        postIdentifierSpace = None,
        tuple = None,
        postTupleSpace = None
      )
  }

  "parse annotation tuples" in {
    val annotation =
      parseAnnotation("@anno(a, b, c + d)")

    annotation shouldBe
      SoftAST.Annotation(
        index = indexOf(">>@anno(a, b, c + d)<<"),
        at = At(">>@<<anno(a, b, c + d)"),
        preIdentifierSpace = None,
        identifier = Identifier("@>>anno<<(a, b, c + d)"),
        postIdentifierSpace = None,
        // No need to test the AST for the Tuple. Simply test that a tuple is defined
        tuple = Some(annotation.tuple.value),
        postTupleSpace = None
      )
  }

  "parse annotation documentation" in {
    val code =
      """// documentation
        |@anno
        |""".stripMargin

    val annotation =
      parseAnnotation(code)

    val expectedComment =
      findFirstComment(annotation).value

    // Test that the Comment tree's code is parsed.
    // No need to assert the comments AST here.
    // These test-cases are for annotations.
    expectedComment.toCode() shouldBe
      """// documentation
        |""".stripMargin

    annotation shouldBe
      SoftAST.Annotation(
        index = indexOf {
          """>>// documentation
            |@anno
            |<<""".stripMargin
        },
        at = SoftAST.TokenDocumented(
          indexOf {
            """>>// documentation
              |@<<anno
              |""".stripMargin
          },
          documentation =
            // The actual Comments AST is not tested here.
            // These test-cases are for annotations.
            // The behaviour of Comments is tested in CommentsSpec
            Some(expectedComment),
          code = SoftAST.CodeToken(
            index = indexOf {
              """// documentation
                |>>@<<anno
                |""".stripMargin
            },
            token = Token.At
          )
        ),
        preIdentifierSpace = None,
        identifier = Identifier {
          """// documentation
            |@>>anno<<
            |""".stripMargin
        },
        postIdentifierSpace = Some(
          SoftAST.Space(
            Code(
              index = indexOf {
                """// documentation
                  |@anno>>
                  |<<""".stripMargin
              },
              token = Token.Newline
            )
          )
        ),
        tuple = None,
        postTupleSpace = None
      )
  }

  "annotations without spaces" in {
    val root = parseSoft("@using@using")

    val parts = root.partsNonEmpty
    parts should have size 1

    val expressions = parts.head.asInstanceOf[SoftAST.ExpressionBlock].expressions
    expressions should have size 2

    expressions shouldBe
      Seq(
        SoftAST.Annotation(
          index = indexOf(">>@using<<@using"),
          at = At(">>@<<using@using"),
          preIdentifierSpace = None,
          identifier = Identifier("@>>using<<@using"),
          postIdentifierSpace = None,
          tuple = None,
          postTupleSpace = None
        ),
        SoftAST.Annotation(
          index = indexOf("@using>>@using<<"),
          at = At("@using>>@<<using"),
          preIdentifierSpace = None,
          identifier = Identifier("@using@>>using<<"),
          postIdentifierSpace = None,
          tuple = None,
          postTupleSpace = None
        )
      )

  }

}
