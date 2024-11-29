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
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._

class ArgumentSpec extends AnyWordSpec with Matchers {

  "parseOrFail" should {
    "fail" when {
      "no open parens are provided" in {
        // If an open paren is not provided, then the following syntax might now be an argument
        val argument =
          parseOrFailArgument("")
            .left
            .value

        // this error message is from FastParse
        argument.message shouldBe """Expected "(""""
      }
    }

    "succeed" when {
      "open parens is provided but argument is missing" in {
        // if an open paren is provided, the syntax is for an argument.
        // Tokens following `(` must be parsed
        val argument =
          parseOrFailArgument("(").value

        argument.openParen shouldBe SoftAST.OpenParen(indexOf(">>(<<"))
        argument.headArgument shouldBe empty
        argument.tailArguments shouldBe empty
        argument.closeParen shouldBe SoftAST.CloseParenExpected(indexOf("(>><<"))
      }
    }
  }

  "parse" when {
    "named arguments" when {
      "single" when {
        "missing closing paren" in {
          val argument =
            parseOrFailArgument("(one").value

          argument.openParen shouldBe SoftAST.OpenParen(indexOf(">>(<<one"))
          argument.headArgument shouldBe Some(SoftAST.Argument("one", indexOf("(>>one<<")))
          argument.tailArguments shouldBe empty
          argument.closeParen shouldBe SoftAST.CloseParenExpected(indexOf("(one>><<"))
        }

        "closing paren is provided" in {
          val argument =
            parseOrFailArgument("(one)").value

          argument.openParen shouldBe SoftAST.OpenParen(indexOf(">>(<<one)"))
          argument.headArgument shouldBe Some(SoftAST.Argument("one", indexOf("(>>one<<)")))
          argument.tailArguments shouldBe empty
          argument.closeParen shouldBe SoftAST.CloseParen(indexOf("(one>>)<<"))
        }
      }

      "multiple" when {
        "missing closing paren" in {
          val argument =
            parseOrFailArgument("(one, two").value

          argument.openParen shouldBe SoftAST.OpenParen(indexOf(">>(<<one, two"))
          argument.headArgument shouldBe Some(SoftAST.Argument("one", indexOf("(>>one<<")))

          argument.tailArguments should have size 1
          argument.tailArguments.head shouldBe
            SoftAST.TailArgument(
              index = indexOf("(one>>, two<<"),
              comma = SoftAST.Comma(indexOf("(one>>,<< two")),
              preArgumentSpace = Some(SoftAST.Space(" ", indexOf("(one,>> <<two"))),
              argument = SoftAST.Argument("two", indexOf("(one, >>two<<")),
              postArgumentSpace = None
            )

          argument.closeParen shouldBe SoftAST.CloseParenExpected(indexOf("(one, two>><<"))
        }

        "comma is provided but the argument is missing" when {
          "second argument is missing" in {
            val argument =
              parseOrFailArgument("(one, ").value

            argument.openParen shouldBe SoftAST.OpenParen(indexOf(">>(<<one, "))
            argument.headArgument shouldBe Some(SoftAST.Argument("one", indexOf("(>>one<<")))

            argument.tailArguments should have size 1
            argument.tailArguments.head shouldBe
              SoftAST.TailArgument(
                index = indexOf("(one>>, <<"),
                comma = SoftAST.Comma(indexOf("(one>>,<< ")),
                preArgumentSpace = Some(SoftAST.Space(" ", indexOf("(one,>> <<"))),
                argument = SoftAST.ArgumentExpected(indexOf("(one, >><<")),
                postArgumentSpace = None
              )

            argument.closeParen shouldBe SoftAST.CloseParenExpected(indexOf("(one, >><<"))
          }

          "first argument is missing" in {
            val argument =
              parseOrFailArgument("(, two").value

            argument.openParen shouldBe SoftAST.OpenParen(indexOf(">>(<<, two"))
            argument.headArgument shouldBe Some(SoftAST.ArgumentExpected(indexOf("(>><<, two")))

            argument.tailArguments should have size 1
            argument.tailArguments.head shouldBe
              SoftAST.TailArgument(
                index = indexOf("(>>, two<<"),
                comma = SoftAST.Comma(indexOf("(>>,<< two")),
                preArgumentSpace = Some(SoftAST.Space(" ", indexOf("(,>> <<two"))),
                argument = SoftAST.Argument("two", indexOf("(, >>two<<")),
                postArgumentSpace = None
              )

            argument.closeParen shouldBe SoftAST.CloseParenExpected(indexOf("(, two>><<"))
          }
        }
      }
    }

    "tupled arguments" when {
      "missing closing paren" in {
        val argument =
          parseOrFailArgument("(one, (two, three))").value

        argument.openParen shouldBe SoftAST.OpenParen(indexOf(">>(<<one, (two, three))"))
        argument.headArgument shouldBe Some(SoftAST.Argument("one", indexOf("(>>one<<, (two, three))")))

        argument.tailArguments should have size 1
        argument.tailArguments.head shouldBe
          SoftAST.TailArgument(
            index = indexOf("(one>>, (two, three)<<)"),
            comma = SoftAST.Comma(indexOf("(one>>,<< (two, three))")),
            preArgumentSpace = Some(SoftAST.Space(" ", indexOf("(one,>> <<(two, three))"))),
            argument = SoftAST.Arguments(
              index = indexOf("(one, >>(two, three)<<)"),
              openParen = SoftAST.OpenParen(indexOf("(one, >>(<<two, three))")),
              preHeadArgumentSpace = None,
              headArgument = Some(SoftAST.Argument("two", indexOf("(one, (>>two<<, three))"))),
              tailArguments = Seq(
                SoftAST.TailArgument(
                  index = indexOf("(one, (two>>, three<<))"),
                  comma = SoftAST.Comma(indexOf("(one, (two>>,<< three))")),
                  preArgumentSpace = Some(SoftAST.Space(" ", indexOf("(one, (two,>> <<three))"))),
                  argument = SoftAST.Argument("three", indexOf("(one, (two, >>three<<))")),
                  postArgumentSpace = None
                )
              ),
              closeParen = SoftAST.CloseParen(indexOf("(one, (two, three>>)<<)"))
            ),
            postArgumentSpace = None
          )

        argument.closeParen shouldBe SoftAST.CloseParen(indexOf("(one, (two, three)>>)<<"))
      }
    }

  }

}
