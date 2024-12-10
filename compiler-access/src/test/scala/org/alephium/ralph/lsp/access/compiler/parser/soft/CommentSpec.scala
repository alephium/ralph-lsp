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
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CommentSpec extends AnyWordSpec with Matchers {

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
              doubleForwardSlash = SoftAST.DoubleForwardSlash(
                code = SoftAST.Code(
                  index = indexOf(">>//<<"),
                  text = "//"
                )
              ),
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
              doubleForwardSlash = SoftAST.DoubleForwardSlash(
                code = SoftAST.Code(
                  index = indexOf(">>//<< "),
                  text = Token.DoubleForwardSlash.lexeme
                )
              ),
              Some(
                SoftAST.Space(
                  code = SoftAST.Code(
                    index = indexOf("//>> <<"),
                    text = " "
                  )
                )
              ),
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
              doubleForwardSlash = SoftAST.DoubleForwardSlash(
                code = SoftAST.Code(
                  index = indexOf(">>//<< my comment \n"),
                  text = "//"
                )
              ),
              preTextSpace = Some(
                SoftAST.Space(
                  code = SoftAST.Code(
                    index = indexOf("//>> <<my comment \n"),
                    text = " "
                  )
                )
              ),
              text = Some(
                SoftAST.Code(
                  index = indexOf("// >>my comment <<\n"),
                  text = "my comment "
                )
              ),
              postTextSpace = Some(
                SoftAST.Space(
                  code = SoftAST.Code(
                    index = indexOf("// my comment >>\n<<"),
                    text = "\n"
                  )
                )
              )
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
              doubleForwardSlash = SoftAST.DoubleForwardSlash(
                code = SoftAST.Code(
                  index = indexOf(">>//<< fn function()"),
                  text = "//"
                )
              ),
              preTextSpace = Some(
                SoftAST.Space(
                  code = SoftAST.Code(
                    index = indexOf("//>> <<fn function()"),
                    text = " "
                  )
                )
              ),
              text = Some(
                SoftAST.Code(
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

}
