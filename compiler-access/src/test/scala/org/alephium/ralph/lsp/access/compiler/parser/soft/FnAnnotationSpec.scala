// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
        at = At {
          """>>@<<
            |fn function() -> ()
            |""".stripMargin
        },
        preIdentifierSpace = Some(
          Space {
            """@>>
              |<<fn function() -> ()
              |""".stripMargin
          }
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
          At {
            """>>@<<annotation
              |fn function() -> ()
              |""".stripMargin
          },
          preIdentifierSpace = None,
          identifier = Identifier {
            """@>>annotation<<
              |fn function() -> ()
              |""".stripMargin
          },
          postIdentifierSpace = Some(
            Space {
              """@annotation>>
                |<<fn function() -> ()
                |""".stripMargin
            }
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
          at = At {
            """>>@<< annotation (a, b + c, c = 4)
              |@ last ()
              |fn function() -> ()
              |""".stripMargin
          },
          preIdentifierSpace = Some(
            Space {
              """@>> <<annotation (a, b + c, c = 4)
                |@ last ()
                |fn function() -> ()
                |""".stripMargin
            }
          ),
          identifier = Identifier {
            """@ >>annotation<< (a, b + c, c = 4)
              |@ last ()
              |fn function() -> ()
              |""".stripMargin
          },
          postIdentifierSpace = Some(
            Space {
              """@ annotation>> <<(a, b + c, c = 4)
                |@ last ()
                |fn function() -> ()
                |""".stripMargin
            }
          ),
          tuple =
            // This test case is not targeting Tuples AST, simply parse it.
            Some(findAnnotation("annotation")(code).flatMap(_.tuple).value),
          postTupleSpace = Some(
            Space {
              """@ annotation (a, b + c, c = 4)>>
                |<<@ last ()
                |fn function() -> ()
                |""".stripMargin
            }
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
          at = At {
            """@ annotation (a, b + c, c = 4)
              |>>@<< last ()
              |fn function() -> ()
              |""".stripMargin
          },
          preIdentifierSpace = Some(
            Space {
              """@ annotation (a, b + c, c = 4)
                |@>> <<last ()
                |fn function() -> ()
                |""".stripMargin
            }
          ),
          identifier = Identifier {
            """@ annotation (a, b + c, c = 4)
              |@ >>last<< ()
              |fn function() -> ()
              |""".stripMargin
          },
          postIdentifierSpace = Some(
            Space {
              """@ annotation (a, b + c, c = 4)
                |@ last>> <<()
                |fn function() -> ()
                |""".stripMargin
            }
          ),
          tuple =
            // This test case is not targeting Tuples AST, simply parse it.
            Some(findAnnotation("last")(code).flatMap(_.tuple).value),
          postTupleSpace = Some(
            Space {
              """@ annotation (a, b + c, c = 4)
                |@ last ()>>
                |<<fn function() -> ()
                |""".stripMargin
            }
          )
        )

    }
  }

}
