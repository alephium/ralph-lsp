package org.alephium.ralph.lsp.access.compiler.ast

import org.alephium.ralph.{Ast, CompilerOptions}
import org.alephium.ralph.lsp.access.compiler.RalphCompilerAccess
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues._
import org.scalatest.wordspec.AnyWordSpec
import java.net.URI

class InconsistentWarningsFixSpec extends AnyWordSpec with Matchers {

  "InconsistentWarningsFix" should {
    "fix inconsistent warnings when compiling twice the same AST" in {
      val testURI = URI.create("file:///a.txt")

      val contract =
        """
        |Contract MyContract(interface: MyInterface) {
        |  pub fn getBool() -> Bool {
        |    return interface.getBool()
        |  }
        |}
        |""".stripMargin

      val interface =
        """
        |Interface MyInterface {
        |   pub fn getBool() -> Bool
        |}
        |""".stripMargin

      val script =
        """
        |TxScript MyScript(x: U256, y: U256) {
        |  assert!(x != y, 0)
        |
        |  fn boolean() -> Bool {
        |    return true
        |  }
        |}
        |""".stripMargin

      def parseOne(code: String): Ast.ContractWithState = {
        val result =
          RalphCompilerAccess.parseContracts(testURI, code).value.statements.collect {
            case source: Tree.Source =>
              source.ast
          }

        result should have size 1
        result.head.left.value
      }

      def parseAll() = {
        val contractAst  = parseOne(contract)
        val interfaceAst = parseOne(interface)
        val scriptAst    = parseOne(script)

        Seq(contractAst, interfaceAst, scriptAst)
      }

      val parsedAST = parseAll()

      val result1 =
        RalphCompilerAccess
          .compileContracts(
            contracts = parsedAST,
            structs = Seq.empty,
            options = CompilerOptions.Default,
            workspaceErrorURI = testURI
          )
          .value

      val result2 =
        RalphCompilerAccess
          .compileContracts(
            contracts = parsedAST,
            structs = Seq.empty,
            options = CompilerOptions.Default,
            workspaceErrorURI = testURI
          )
          .value

      val (statefulContracts1, statefulScripts1) = result1
      val (statefulContracts2, statefulScripts2) = result2

      statefulContracts1 shouldBe statefulContracts2
      statefulScripts1 shouldBe statefulScripts2

      val result1Warnings: Array[String] =
        statefulContracts1.flatMap(_.warnings) ++
          statefulScripts1.flatMap(_.warnings)

      val result2Warnings: Array[String] =
        statefulContracts2.flatMap(_.warnings) ++
          statefulScripts2.flatMap(_.warnings)

      result1Warnings.size should be > 0

      result1Warnings shouldBe result2Warnings
    }
  }

}
