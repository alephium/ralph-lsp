// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class CommentParserSpec extends AnyWordSpec with Matchers {

  "no text comment" should {
    "store empty comment" in {
      val comment =
        parseComment("//")

      val expected =
        SoftAST.Comments(
          index = indexOf(">>//<<"),
          preCommentSpace = None,
          comments = Seq(
            SoftAST.Comment(
              index = indexOf(">>//<<"),
              doubleForwardSlash = DoubleForwardSlash(">>//<<"),
              preTextSpace = None,
              text = None,
              postTextSpace = None
            )
          ),
          postCommentSpace = None
        )

      comment shouldBe expected
    }
  }

  "space as comment" should {
    "store the space and empty comment" in {
      val comment =
        parseComment("// ")

      val expected =
        SoftAST.Comments(
          index = indexOf(">>// <<"),
          preCommentSpace = None,
          comments = Seq(
            SoftAST.Comment(
              index = indexOf(">>// <<"),
              doubleForwardSlash = DoubleForwardSlash(">>//<< "),
              preTextSpace = Some(Space("//>> <<")),
              text = None,
              postTextSpace = None
            )
          ),
          postCommentSpace = None
        )

      comment shouldBe expected
    }
  }

  "text as comment" should {
    "store the space and the comment" in {
      val newLine =
        Token.Newline.lexeme

      val comment =
        parseComment(s"// my comment $newLine")

      val expected =
        SoftAST.Comments(
          index = indexOf(s">>// my comment $newLine<<"),
          preCommentSpace = None,
          comments = Seq(
            SoftAST.Comment(
              index = indexOf(s">>// my comment <<$newLine"),
              doubleForwardSlash = DoubleForwardSlash(s">>//<< my comment $newLine"),
              preTextSpace = Some(Space(s"//>> <<my comment $newLine")),
              text = Some(
                SoftAST.CodeString(
                  index = indexOf(s"// >>my comment <<$newLine"),
                  text = "my comment "
                )
              ),
              postTextSpace = None
            )
          ),
          postCommentSpace = Some(Space(s"// my comment >>$newLine<<"))
        )

      comment shouldBe expected
    }
  }

  "code as comment" should {
    "not parse the comment as code, but as text" in {
      val code =
        "// fn function()"

      val comment =
        parseComment(code)

      val expected =
        SoftAST.Comments(
          index = indexOf(s">>$code<<"),
          preCommentSpace = None,
          comments = Seq(
            SoftAST.Comment(
              index = indexOf(s">>$code<<"),
              doubleForwardSlash = DoubleForwardSlash(">>//<< fn function()"),
              preTextSpace = Some(Space("//>> <<fn function()")),
              text = Some(
                SoftAST.CodeString(
                  index = indexOf("// >>fn function()<<"),
                  text = "fn function()"
                )
              ),
              postTextSpace = None
            )
          ),
          postCommentSpace = None
        )

      comment shouldBe expected
    }
  }

  "comments should ALWAYS parse single line (newlines make a new comment)" when {
    "texts within the comment are empty" when {
      "a comment is provided before each token within an expression" in {
        val comment =
          parseSoft {
            """
              |//
              |let
              |//
              |counter
              |//
              |=
              |//
              |0
              |""".stripMargin
          }

        val parts = comment.partsNonEmpty
        parts should have size 1
        val varDec = parts.head.asInstanceOf[SoftAST.VariableDeclaration]

        /**
         * Assert Errors: There should be no errors because the expression `let counter = 0` is valid.
         */
        val errors =
          varDec
            .toNode
            .walkDown
            .map(_.data)
            .collect { // collect all errors
              case error: SoftAST.ErrorAST =>
                error
            }
            .toList

        errors shouldBe empty // there are no errors

        /**
         * Assert Comments: All text within the comment should be empty
         */
        val comments =
          varDec
            .toNode
            .walkDown
            .map(_.data)
            .collect { // collect all comment texts
              case comment: SoftAST.Comment =>
                comment.text
            }
            .toList

        comments should have size 4     // there are 4 comments
        comments.flatten shouldBe empty // there are no comment texts
      }

      "single comment is provided before an expression" in {
        val comment =
          parseSoft {
            """
              |//
              |let counter = 0
              |""".stripMargin
          }

        val parts = comment.partsNonEmpty
        parts should have size 1
        val varDec = parts.head.asInstanceOf[SoftAST.VariableDeclaration]

        varDec.let.documentation.value shouldBe
          SoftAST.Comments(
            index = indexOf {
              """
                |>>//
                |<<let counter = 0
                |""".stripMargin
            },
            preCommentSpace = None,
            comments = Seq(
              SoftAST.Comment(
                index = indexOf {
                  """
                    |>>//<<
                    |let counter = 0
                    |""".stripMargin
                },
                doubleForwardSlash = DoubleForwardSlash {
                  indexOf {
                    """
                      |>>//<<
                      |let counter = 0
                      |""".stripMargin
                  }
                },
                preTextSpace = None,
                text = None,
                postTextSpace = None
              )
            ),
            postCommentSpace = Some(
              Space {
                """
                  |//>>
                  |<<let counter = 0
                  |""".stripMargin
              }
            )
          )
      }

      "two comments are provided before an expression" in {
        val comment =
          parseSoft {
            """
              |//
              |//
              |let counter = 0
              |""".stripMargin
          }

        val parts = comment.partsNonEmpty
        parts should have size 1
        val varDec = parts.head.asInstanceOf[SoftAST.VariableDeclaration]

        varDec.let.documentation.value shouldBe
          SoftAST.Comments(
            index = indexOf {
              """
                |>>//
                |//
                |<<let counter = 0
                |""".stripMargin
            },
            preCommentSpace = None,
            comments = Seq(
              SoftAST.Comment(
                index = indexOf {
                  """
                    |>>//
                    |<<//
                    |let counter = 0
                    |""".stripMargin
                },
                doubleForwardSlash = DoubleForwardSlash {
                  indexOf {
                    """
                      |>>//<<
                      |//
                      |let counter = 0
                      |""".stripMargin
                  }
                },
                preTextSpace = None,
                text = None,
                postTextSpace = Some(
                  Space {
                    """
                      |//>>
                      |<<//
                      |let counter = 0
                      |""".stripMargin
                  }
                )
              ),
              SoftAST.Comment(
                index = indexOf {
                  """
                    |//
                    |>>//<<
                    |let counter = 0
                    |""".stripMargin
                },
                doubleForwardSlash = DoubleForwardSlash {
                  indexOf {
                    """
                      |//
                      |>>//<<
                      |let counter = 0
                      |""".stripMargin
                  }
                },
                preTextSpace = None,
                text = None,
                postTextSpace = None
              )
            ),
            postCommentSpace = Some(
              Space {
                """
                  |//
                  |//>>
                  |<<let counter = 0
                  |""".stripMargin
              }
            )
          )
      }
    }
  }

}
