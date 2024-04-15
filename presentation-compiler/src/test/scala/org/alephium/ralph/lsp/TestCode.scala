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

package org.alephium.ralph.lsp

import org.alephium.ralph.lsp.TestCommon.genCamelCase
import org.scalacheck.Gen

import scala.util.Random

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

  def genExtendedContract(
      name: Gen[String] = genCamelCase,
      extensionName: Gen[String] = genCamelCase): Gen[(String, String, String)] =
    for {
      name          <- name
      extensionName <- extensionName
      extension     <- genAbstract(Gen.const(extensionName))
    } yield {
      val contract = s"""
         |Contract $name(id:U256) extends $extensionName() {
         |  pub fn action() -> () {
         |    return
         |  }
         |}
         |""".stripMargin

      (contract, extension, extensionName)
    }

  /** Generate ralph code */
  def genGoodCode(): Gen[String] =
    Gen.oneOf(
      genContract(genCamelCase),
      genAbstract(genCamelCase),
      genInterface(genCamelCase),
      genScript(genCamelCase)
    )

  def genBadCode(): Gen[String] =
    Gen.oneOf(
      genContract(genCamelCase.map(_.toLowerCase)),
      genAbstract(genCamelCase.map(_.toLowerCase)),
      genInterface(genCamelCase.map(_.toLowerCase)),
      genScript(genCamelCase.map(_.toLowerCase))
    )

  def genGoodOrBad(): Gen[String] =
    Gen.oneOf(
      TestCode.genGoodCode(),
      TestCode.genBadCode()
    )

  def genAtLeastOneBadCode(): Gen[List[String]] =
    for {
      goodAndBad <- Gen.listOf(genGoodOrBad())
      bad        <- genBadCode()
    } yield Random.shuffle(goodAndBad :+ bad)

}
