package org.alephium.ralph.lsp

import org.alephium.ralph.lsp.TestCommon.genCamelCase
import org.scalacheck.Gen

/** Ralph source code related test functions */
object TestCode {

  def genContract(name: Gen[String] = genCamelCase): Gen[String] =
    name map {
      name =>
        s"""
           |Contract $name(id:U256){
           |  pub fn getId() -> U256 {
           |    return id
           |  }
           |}
           |""".stripMargin
    }

  def genScript(name: Gen[String] = genCamelCase): Gen[String] =
    name map {
      name =>
        s"""
           |TxScript $name(x: U256, y: U256) {
           |  assert!(x != y, 0)
           |}
           |""".stripMargin
    }

  /** Generate ralph code */
  def genGoodCode(): Gen[String] =
    Gen.oneOf(
      genContract(genCamelCase),
      genScript(genCamelCase),
    )

  def genBadCode(): Gen[String] =
    Gen.oneOf(
      genContract(genCamelCase.map(_.toLowerCase)),
      genScript(genCamelCase.map(_.toLowerCase)),
    )

}