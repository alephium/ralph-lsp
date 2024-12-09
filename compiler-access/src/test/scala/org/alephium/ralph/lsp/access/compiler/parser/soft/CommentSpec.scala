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

import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CommentSpec extends AnyWordSpec with Matchers {

  "single line comments" when {
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
                doubleForwardSlash = SoftAST.DoubleForwardSlash(indexOf(">>//<<")),
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
                doubleForwardSlash = SoftAST.DoubleForwardSlash(indexOf(">>//<< ")),
                Some(SoftAST.Space(" ", indexOf("//>> <<"))),
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
        val comment =
          parseComment("// my comment \n")

        val expected =
          SoftAST.Comments(
            index = indexOf(">>// my comment \n<<"),
            preCommentSpace = None,
            comments = Seq(
              SoftAST.Comment(
                index = indexOf(">>// my comment \n<<"),
                doubleForwardSlash = SoftAST.DoubleForwardSlash(indexOf(">>//<< my comment \n")),
                preTextSpace = Some(SoftAST.Space(" ", indexOf("//>> <<my comment \n"))),
                text = Some(SoftAST.Text("my comment ", indexOf("// >>my comment <<\n"))),
                postTextSpace = Some(SoftAST.Space("\n", indexOf("// my comment >>\n<<")))
              )
            ),
            postCommentSpace = None
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
                doubleForwardSlash = SoftAST.DoubleForwardSlash(indexOf(">>//<< fn function()")),
                preTextSpace = Some(SoftAST.Space(" ", indexOf("//>> <<fn function()"))),
                text = Some(SoftAST.Text("fn function()", indexOf("// >>fn function()<<"))),
                postTextSpace = None
              )
            ),
            postCommentSpace = None
          )

        comment shouldBe expected
      }
    }
  }

  "multi line comments" when {
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
          doubleForwardSlash = SoftAST.DoubleForwardSlash(
            index = indexOf {
              """>>//<< one
                |// two
                |""".stripMargin
            }
          ),
          preTextSpace = Some(
            SoftAST.Space(
              " ",
              indexOf {
                """//>> <<one
                  |// two
                  |""".stripMargin
              }
            )
          ),
          text = Some(
            SoftAST.Text(
              code = "one",
              index = indexOf {
                """// >>one<<
                  |// two
                  |""".stripMargin
              }
            )
          ),
          postTextSpace = Some(
            SoftAST.Space(
              code = "\n",
              index = indexOf {
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
          doubleForwardSlash = SoftAST.DoubleForwardSlash(
            index = indexOf {
              """// one
                |>>//<< two
                |""".stripMargin
            }
          ),
          preTextSpace = Some(
            SoftAST.Space(
              " ",
              indexOf {
                """// one
                  |//>> <<two
                  |""".stripMargin
              }
            )
          ),
          text = Some(
            SoftAST.Text(
              code = "two",
              index = indexOf {
                """// one
                  |// >>two<<
                  |""".stripMargin
              }
            )
          ),
          postTextSpace = Some(
            SoftAST.Space(
              code = "\n",
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
                  doubleForwardSlash = SoftAST.DoubleForwardSlash(
                    index = indexOf(s">>//<< //")
                  ),
                  preTextSpace = Some(
                    SoftAST.Space(
                      code = " ",
                      index = indexOf(s"//>> <<//")
                    )
                  ),
                  text = Some(
                    SoftAST.Text(
                      code = "//",
                      index = indexOf(s"// >>//<<")
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
                  doubleForwardSlash = SoftAST.DoubleForwardSlash(
                    index = indexOf {
                      """>>//<<
                        |//""".stripMargin
                    }
                  ),
                  preTextSpace = Some(
                    SoftAST.Space(
                      code = "\n",
                      index = indexOf {
                        """//>>
                          |<<//""".stripMargin
                      }
                    )
                  ),
                  text = Some(
                    SoftAST.Text(
                      code = "//",
                      index = indexOf {
                        """//
                          |>>//<<""".stripMargin
                      }
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

}
