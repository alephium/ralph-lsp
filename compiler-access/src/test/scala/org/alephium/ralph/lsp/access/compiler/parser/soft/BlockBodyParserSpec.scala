package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BlockBodyParserSpec extends AnyWordSpec with Matchers {

  "not wrap a single expression within an ExpressionBlock" when {
    "only one expression exists" when {
      def doTest(body: SoftAST.BlockBody) = {
        // the body contains only a single element
        body.parts should have size 1
        // that element is a `VariableDeclaration` and not an `ExpressionBlock`.
        val variableDeclaration = body.parts.head.part.asInstanceOf[SoftAST.VariableDeclaration]
        variableDeclaration.toCode() shouldBe "let one = 1"
      }

      "block is parsed as root" in {
        doTest {
          parseBlockBodyRoot {
            """
              |let one = 1
              |""".stripMargin
          }
        }
      }

      "block is parsed as child" in {
        doTest {
          parseBlockBodyChild {
            """
              |let one = 1
              |""".stripMargin
          }
        }
      }
    }
  }

  "two expressions" should {
    "wrap multiple expression within ExpressionBlock" when {
      "block is parsed as root" in {
        val body =
          parseBlockBodyRoot {
            """
              |let one = 1
              |true == true
              |""".stripMargin
          }

        // now that the body contains two expressions, it should wrap it in an ExpressionBlock
        // so it still contains only one element
        body.parts should have size 1
        val expressionBlock = body.parts.head.part.asInstanceOf[SoftAST.ExpressionBlock]

        // head expression contains the variable declaration
        expressionBlock.headExpression shouldBe a[SoftAST.VariableDeclaration]
        expressionBlock.headExpression.toCode() shouldBe "let one = 1"

        // tail expression also has size one as it contains only the infix equal check
        expressionBlock.tailExpressions should have size 1
        expressionBlock.tailExpressions.head.expression shouldBe a[SoftAST.InfixExpression]
        expressionBlock.tailExpressions.head.expression.toCode() shouldBe "true == true"
      }
    }

    "not wrap multiple expression within ExpressionBlock" when {
      "block is parsed as a child" in {
        val body =
          parseBlockBodyChild {
            """
              |let one = 1
              |true == true
              |""".stripMargin
          }

        // each expression is parsed as an individual expression/body-part i.e. it does not get wrapped into a block
        body.parts should have size 2

        // head expression contains the variable declaration
        body.parts.head.part shouldBe a[SoftAST.VariableDeclaration]
        body.parts.head.part.toCode() shouldBe "let one = 1"

        // last expression contains the infix operation
        body.parts.last.part shouldBe a[SoftAST.InfixExpression]
        body.parts.last.part.toCode() shouldBe "true == true"
      }
    }
  }

}
