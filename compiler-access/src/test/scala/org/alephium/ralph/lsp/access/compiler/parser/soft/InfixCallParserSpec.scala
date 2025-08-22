// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InfixCallParserSpec extends AnyWordSpec with Matchers {

  "succeed" when {
    "left & right expressions are tuples" in {
      val infix =
        parseInfixCall("(one + one) <= (this - that)")

      val left = infix.leftExpression.asInstanceOf[SoftAST.Group[_, _, _]]
      left.toCode() shouldBe "(one + one)"

      val right = infix.rightExpression.asInstanceOf[SoftAST.Group[_, _, _]]
      right.toCode() shouldBe "(this - that)"

      infix.operator shouldBe LessThanOrEqual("(one + one) >><=<< (this - that)")
    }
  }

  "comment exists before infix operator" in {
    val infix =
      parseInfixCall(
        """one
          |// comment
          |+ one""".stripMargin
      )

    infix shouldBe
      SoftAST.InfixExpression(
        index = indexOf(
          """>>one
            |// comment
            |+ one<<""".stripMargin
        ),
        leftExpression = Identifier(
          """>>one<<
            |// comment
            |+ one""".stripMargin
        ),
        preOperatorSpace = Some(
          Space(
            """one>>
              |<<// comment
              |+ one""".stripMargin
          )
        ),
        operator = SoftAST.TokenDocumented(
          index = indexOf(
            """one
              |>>// comment
              |+<< one""".stripMargin
          ),
          documentation = Some(
            SoftAST.Comments(
              index = indexOf(
                """one
                  |>>// comment
                  |<<+ one""".stripMargin
              ),
              preCommentSpace = None,
              comments = Seq(
                SoftAST.Comment(
                  index = indexOf(
                    """one
                      |>>// comment<<
                      |+ one""".stripMargin
                  ),
                  doubleForwardSlash = DoubleForwardSlash(
                    """one
                      |>>//<< comment
                      |+ one""".stripMargin
                  ),
                  preTextSpace = Some(
                    Space(
                      """one
                      |//>> <<comment
                      |+ one""".stripMargin
                    )
                  ),
                  text = Some(
                    CodeString(
                      """one
                      |// >>comment<<
                      |+ one""".stripMargin
                    )
                  ),
                  postTextSpace = None
                )
              ),
              postCommentSpace = Some(
                Space(
                  """one
                    |// comment>>
                    |<<+ one""".stripMargin
                )
              )
            )
          ),
          code = Plus(
            """one
                |// comment
                |>>+<< one""".stripMargin
          ).code
        ),
        postOperatorSpace = Some(
          Space(
            """one
            |// comment
            |+>> <<one""".stripMargin
          )
        ),
        rightExpression = Identifier(
          """one
              |// comment
              |+ >>one<<""".stripMargin
        )
      )

  }

  "parse all infix operations" in {
    Token.infix foreach {
      infixToken =>
        val operation = infixToken.lexeme

        val ast =
          parseInfixCall(s"""-1i $operation 2i""")

        // Use empty spaces of the same length as the infix-operation instead of the actual operation
        // to avoid conflicts with the symbol `>> <<` when `a >> b` is used which results in `>>>><<`.
        val operationSpace = " " * operation.length

        ast shouldBe
          SoftAST.InfixExpression(
            index = indexOf(s""">>-1i $operationSpace 2i<<"""),
            leftExpression = Number(s""">>-1i<< $operationSpace 2i"""),
            preOperatorSpace = Some(Space(s"""-1i>> <<$operationSpace 2i""")),
            operator = TokenDocumented(code = s"""-1i >>$operationSpace<< 2i""", token = infixToken),
            postOperatorSpace = Some(Space(s"""-1i $operationSpace>> <<2i""")),
            rightExpression = Number(s"""-1i $operationSpace >>2i<<""")
          )
    }
  }

}
