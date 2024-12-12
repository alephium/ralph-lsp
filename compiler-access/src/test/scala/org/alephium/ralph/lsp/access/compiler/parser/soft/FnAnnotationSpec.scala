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
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class FnAnnotationSpec extends AnyWordSpec with Matchers {

  "annotations are not defined" in {
    val function =
      parseFunction("fn function() -> ()")

    function.annotations shouldBe empty
  }

  "report missing identifier" in {
    val function =
      parseFunction {
        """@
          |fn function() -> ()
          |""".stripMargin
      }

    function.annotations should have size 1
    function.annotations.head shouldBe
      SoftAST.Annotation(
        indexOf {
          """>>@
            |<<fn function() -> ()
            |""".stripMargin
        },
        at = At(
          indexOf {
            """>>@<<
              |fn function() -> ()
              |""".stripMargin
          }
        ),
        preIdentifierSpace = Some(
          SpaceNewline(
            indexOf {
              """@>>
                |<<fn function() -> ()
                |""".stripMargin
            }
          )
        ),
        identifier = SoftAST.IdentifierExpected(
          indexOf {
            """@
              |>><<fn function() -> ()
              |""".stripMargin
          }
        ),
        postIdentifierSpace = None,
        tuple = None,
        postTupleSpace = None
      )
  }

  "annotations are defined" when {
    "without head space and empty parameters" in {
      val function =
        parseFunction {
          """@annotation
            |fn function() -> ()
            |""".stripMargin
        }

      val expected =
        SoftAST.Annotation(
          indexOf {
            """>>@annotation
              |<<fn function() -> ()
              |""".stripMargin
          },
          at = At(
            indexOf {
              """>>@<<annotation
                |fn function() -> ()
                |""".stripMargin
            }
          ),
          preIdentifierSpace = None,
          identifier = Identifier(
            index = indexOf {
              """@>>annotation<<
                |fn function() -> ()
                |""".stripMargin
            },
            text = "annotation"
          ),
          postIdentifierSpace = Some(
            SpaceNewline(
              indexOf {
                """@annotation>>
                  |<<fn function() -> ()
                  |""".stripMargin
              }
            )
          ),
          tuple = None,
          postTupleSpace = None
        )

      function.annotations should have size 1
      function.annotations.head shouldBe expected
    }

    "two annotations with expressions and spaces" in {
      val code =
        """@ annotation (a, b + c, c = 4)
          |@ last ()
          |fn function() -> ()
          |""".stripMargin

      val function =
        parseFunction(code)

      function.annotations should have size 2

      function.annotations.head shouldBe
        SoftAST.Annotation(
          indexOf {
            """>>@ annotation (a, b + c, c = 4)
              |<<@ last ()
              |fn function() -> ()
              |""".stripMargin
          },
          at = At(
            indexOf {
              """>>@<< annotation (a, b + c, c = 4)
                |@ last ()
                |fn function() -> ()
                |""".stripMargin
            }
          ),
          preIdentifierSpace = Some(
            SpaceOne(
              indexOf {
                """@>> <<annotation (a, b + c, c = 4)
                  |@ last ()
                  |fn function() -> ()
                  |""".stripMargin
              }
            )
          ),
          identifier = Identifier(
            index = indexOf {
              """@ >>annotation<< (a, b + c, c = 4)
                |@ last ()
                |fn function() -> ()
                |""".stripMargin
            },
            text = "annotation"
          ),
          postIdentifierSpace = Some(
            SpaceOne(
              index = indexOf {
                """@ annotation>> <<(a, b + c, c = 4)
                  |@ last ()
                  |fn function() -> ()
                  |""".stripMargin
              }
            )
          ),
          tuple =
            // This test case is not targeting Tuples AST, simply parse it.
            Some(findAnnotation("annotation")(code).flatMap(_.tuple).value),
          postTupleSpace = Some(
            SpaceNewline(
              index = indexOf {
                """@ annotation (a, b + c, c = 4)>>
                  |<<@ last ()
                  |fn function() -> ()
                  |""".stripMargin
              }
            )
          )
        )

      function.annotations.last shouldBe
        SoftAST.Annotation(
          indexOf {
            """@ annotation (a, b + c, c = 4)
              |>>@ last ()
              |<<fn function() -> ()
              |""".stripMargin
          },
          at = At(
            indexOf {
              """@ annotation (a, b + c, c = 4)
                |>>@<< last ()
                |fn function() -> ()
                |""".stripMargin
            }
          ),
          preIdentifierSpace = Some(
            SpaceOne(
              index = indexOf {
                """@ annotation (a, b + c, c = 4)
                  |@>> <<last ()
                  |fn function() -> ()
                  |""".stripMargin
              }
            )
          ),
          identifier = Identifier(
            index = indexOf {
              """@ annotation (a, b + c, c = 4)
                |@ >>last<< ()
                |fn function() -> ()
                |""".stripMargin
            },
            text = "last"
          ),
          postIdentifierSpace = Some(
            SpaceOne(
              index = indexOf {
                """@ annotation (a, b + c, c = 4)
                  |@ last>> <<()
                  |fn function() -> ()
                  |""".stripMargin
              }
            )
          ),
          tuple =
            // This test case is not targeting Tuples AST, simply parse it.
            Some(findAnnotation("last")(code).flatMap(_.tuple).value),
          postTupleSpace = Some(
            SpaceNewline(
              indexOf {
                """@ annotation (a, b + c, c = 4)
                  |@ last ()>>
                  |<<fn function() -> ()
                  |""".stripMargin
              }
            )
          )
        )

    }
  }

}
