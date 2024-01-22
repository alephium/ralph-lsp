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

  def genAbstract(name: Gen[String] = genCamelCase): Gen[String] =
    name map {
      name =>
        s"""
           |Abstract Contract $name(){
           |  pub fn action() -> ()
           |}
           |""".stripMargin
    }

  def genInterface(name: Gen[String] = genCamelCase): Gen[String] =
    name map {
      name =>
        s"""
           |Interface $name {
           |  pub fn math() -> U256
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
      genAbstract(genCamelCase),
      genInterface(genCamelCase),
      genScript(genCamelCase),
    )

  def genBadCode(): Gen[String] =
    Gen.oneOf(
      genContract(genCamelCase.map(_.toLowerCase)),
      genAbstract(genCamelCase.map(_.toLowerCase)),
      genInterface(genCamelCase.map(_.toLowerCase)),
      genScript(genCamelCase.map(_.toLowerCase)),
    )

}
