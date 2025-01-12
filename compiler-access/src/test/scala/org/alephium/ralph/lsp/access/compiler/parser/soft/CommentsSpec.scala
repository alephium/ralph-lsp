package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser.parseComment
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CommentsSpec extends AnyWordSpec with Matchers {

  "text comment exist" in {
    val code =
      """// one
        |// two
        |""".stripMargin

    val comment =
      parseComment(code)

    val expectedFirstComment =
      SoftAST.Comment(
        index = indexOf {
          """>>// one
            |<<// two
            |""".stripMargin
        },
        doubleForwardSlash = DoubleForwardSlash(
          indexOf {
            """>>//<< one
              |// two
              |""".stripMargin
          }
        ),
        preTextSpace = Some(
          SpaceOne(
            indexOf {
              """//>> <<one
                |// two
                |""".stripMargin
            }
          )
        ),
        text = Some(
          SoftAST.CodeString(
            index = indexOf {
              """// >>one<<
                |// two
                |""".stripMargin
            },
            text = "one"
          )
        ),
        postTextSpace = Some(
          SpaceNewline(
            indexOf {
              """// one>>
                |<<// two
                |""".stripMargin
            }
          )
        )
      )

    // assert the first comment so failures are easier to debug
    comment.comments.head shouldBe expectedFirstComment

    val expectedSecondComment =
      SoftAST.Comment(
        index = indexOf {
          """// one
            |>>// two
            |<<""".stripMargin
        },
        doubleForwardSlash = DoubleForwardSlash(
          index = indexOf {
            """// one
              |>>//<< two
              |""".stripMargin
          }
        ),
        preTextSpace = Some(
          SpaceOne(
            indexOf {
              """// one
                |//>> <<two
                |""".stripMargin
            }
          )
        ),
        text = Some(
          SoftAST.CodeString(
            index = indexOf {
              """// one
                |// >>two<<
                |""".stripMargin
            },
            text = "two"
          )
        ),
        postTextSpace = Some(
          SpaceNewline(
            index = indexOf {
              """// one
                |// two>>
                |<<""".stripMargin
            }
          )
        )
      )

    // assert the second comment so failures are easier to debug
    comment.comments.last shouldBe expectedSecondComment

    val expected =
      SoftAST.Comments(
        index = indexOf(s">>$code<<"),
        preCommentSpace = None,
        comments = Seq(
          expectedFirstComment,
          expectedSecondComment
        ),
        postCommentSpace = None
      )

    comment shouldBe expected
  }

  "text comments are absent" should {
    "parse the `//` as the comment" when {
      "// is in the same line" in {
        val code = "// //"

        val comment =
          parseComment(code)

        val expected =
          SoftAST.Comments(
            index = indexOf(s">>$code<<"),
            preCommentSpace = None,
            comments = Seq(
              SoftAST.Comment(
                index = indexOf(s">>$code<<"),
                doubleForwardSlash = DoubleForwardSlash(indexOf(s">>//<< //")),
                preTextSpace = Some(SpaceOne(indexOf(s"//>> <<//"))),
                text = Some(
                  SoftAST.CodeString(
                    index = indexOf(s"// >>//<<"),
                    text = "//"
                  )
                ),
                postTextSpace = None
              )
            ),
            postCommentSpace = None
          )

        comment shouldBe expected
      }

      "// is on a new line" in {
        val code =
          """//
            |//""".stripMargin

        val comment =
          parseComment(code)

        val expected =
          SoftAST.Comments(
            index = indexOf(s">>$code<<"),
            preCommentSpace = None,
            comments = Seq(
              SoftAST.Comment(
                index = indexOf(s">>$code<<"),
                doubleForwardSlash = DoubleForwardSlash(
                  indexOf {
                    """>>//<<
                      |//""".stripMargin
                  }
                ),
                preTextSpace = Some(
                  SpaceNewline(
                    indexOf {
                      """//>>
                        |<<//""".stripMargin
                    }
                  )
                ),
                text = Some(
                  SoftAST.CodeString(
                    index = indexOf {
                      """//
                        |>>//<<""".stripMargin
                    },
                    text = "//"
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
  }

}
