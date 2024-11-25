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

  "no comment text" should {
    "store empty comment" in {
      val comment =
        parseComment("//")

      val expected =
        SoftAST.Comment(
          index = indexOf(">>//<<"),
          doubleForwardSlash = SoftAST.DoubleForwardSlash(indexOf(">>//<<")),
          space = None,
          text = None
        )

      comment shouldBe expected
    }
  }

  "space as comment" should {
    "store the space and empty comment" in {
      val comment =
        parseComment("// ")

      val expected =
        SoftAST.Comment(
          index = indexOf(">>// <<"),
          doubleForwardSlash = SoftAST.DoubleForwardSlash(indexOf(">>//<< ")),
          space = Some(SoftAST.Space(" ", indexOf("//>> <<"))),
          text = None
        )

      comment shouldBe expected
    }
  }

  "text as comment" should {
    "store the space and the comment" in {
      val comment =
        parseComment("// my comment")

      val expected =
        SoftAST.Comment(
          index = indexOf(">>// my comment<<"),
          doubleForwardSlash = SoftAST.DoubleForwardSlash(indexOf(">>//<< my comment")),
          space = Some(SoftAST.Space(" ", indexOf("//>> <<my comment"))),
          text = Some(SoftAST.Text("my comment", indexOf("// >>my comment<<")))
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
        SoftAST.Comment(
          index = indexOf(s">>$code<<"),
          doubleForwardSlash = SoftAST.DoubleForwardSlash(indexOf(">>//<< fn function()")),
          space = Some(SoftAST.Space(" ", indexOf("//>> <<fn function()"))),
          text = Some(SoftAST.Text("fn function()", indexOf("// >>fn function()<<")))
        )

      comment shouldBe expected
    }
  }

}
