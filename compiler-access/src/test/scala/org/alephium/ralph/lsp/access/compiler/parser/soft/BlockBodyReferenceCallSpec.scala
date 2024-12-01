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

/** Tests related to [[SoftAST.ReferenceCall]] when [[SoftAST.BlockBody]] is parsed */
class BlockBodyReferenceCallSpec extends AnyWordSpec with Matchers {

  "parse a reference call" in {
    val code =
      """
        |Contract ABC () {}
        |
        |function(one)
        |
        |blah
        |""".stripMargin

    val actual =
      parseBlockBody(code)

    val expected =
      SoftAST.BlockBodyPart(
        index = indexOf {
          """
            |Contract ABC () {}
            |
            |>>function(one)
            |
            |<<blah
            |""".stripMargin
        },
        part = SoftAST.ReferenceCall(
          index = indexOf {
            """
              |Contract ABC () {}
              |
              |>>function(one)<<
              |
              |blah
              |""".stripMargin
          },
          reference = SoftAST.Identifier(
            code = "function",
            index = indexOf {
              """
                |Contract ABC () {}
                |
                |>>function<<(one)
                |
                |blah
                |""".stripMargin
            }
          ),
          preArgumentsSpace = None,
          arguments = SoftAST.Arguments(
            index = indexOf {
              """
                |Contract ABC () {}
                |
                |function>>(one)<<
                |
                |blah
                |""".stripMargin
            },
            openParen = SoftAST.OpenParen(
              indexOf {
                """
                  |Contract ABC () {}
                  |
                  |function>>(<<one)
                  |
                  |blah
                  |""".stripMargin
              }
            ),
            preHeadArgumentSpace = None,
            headArgument = Some(
              SoftAST.Argument(
                code = "one",
                index = indexOf {
                  """
                    |Contract ABC () {}
                    |
                    |function(>>one<<)
                    |
                    |blah
                    |""".stripMargin
                }
              )
            ),
            tailArguments = Seq.empty,
            closeParen = SoftAST.CloseParen(indexOf {
              """
                |Contract ABC () {}
                |
                |function(one>>)<<
                |
                |blah
                |""".stripMargin
            })
          )
        ),
        postPartSpace = Some(
          SoftAST.Space(
            code = Token.Newline.lexeme * 2, // Newline twice
            index = indexOf {
              """
                |Contract ABC () {}
                |
                |function(one)>>
                |
                |<<blah
                |""".stripMargin
            }
          )
        )
      )

    actual.parts should contain(expected)
  }

}
