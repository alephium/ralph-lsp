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

package org.alephium.ralph.lsp.server

import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import fastparse._ //, NoWhitespace._

import org.alephium.ralph.StatefulParser
import org.alephium.ralph.lsp.access.file.FileAccess

class DefaultConfigSpec extends AnyWordSpec with Matchers with MockFactory with ScalaFutures {

  implicit val whitespace: fastparse.Whitespace =
    new StatefulParser(None).RalphWhitespace

  implicit val file: FileAccess =
    FileAccess.disk

  val defaultNodeCompilerOptions = "export const DEFAULT_NODE_COMPILER_OPTIONS: node.CompilerOptions = "

  val defaultCompilerOptions = "export const DEFAULT_COMPILER_OPTIONS: CompilerOptions = "

  def bool[_: P] = P("true" | "false").!.map(_.toBoolean)

  def substitueValue[_: P]: P[Option[(String, Boolean)]] = P("..." ~ CharIn("A-Za-z_").rep).map(
    _ => None
  )

  def keyValue[_: P]: P[Option[(String, Boolean)]] = P(CharIn("A-Za-z").rep.! ~ ":" ~ bool).map(Some)

  def objBody[_: P]: P[Map[String, Boolean]] = P(
    (keyValue | substitueValue)
      .rep(sep = ",")
      .map(_.collect {
        case Some(v) => v
      }.toMap)
  )

  def obj[_: P]: P[Map[String, Boolean]] = P("{" ~ objBody ~ "}")

  def parser[_: P]: P[Map[String, Boolean]] = P(obj ~ defaultCompilerOptions ~ obj).map {
    case (a, b) => a ++ b
  }

  "Parser" should {
    "read default config" in {

      val uri         = new java.io.File("target/web3/alephium-web3-0.38.0/packages/web3/src/contract/contract.ts").toURI
      val fileContent = file.read(uri).toOption.get

      // Find whenr the default config starts
      val index = fileContent.indexOf(defaultNodeCompilerOptions)

      // Extract the start of the default config
      val config = fileContent.drop(index + defaultNodeCompilerOptions.length)

      val result = fastparse.parse(config, parser(_)).get.value

      result shouldBe Map(
        ("ignoreUnusedConstantsWarnings", false),
        ("ignoreUnusedVariablesWarnings", false),
        ("ignoreUnusedFieldsWarnings", false),
        ("ignoreUnusedPrivateFunctionsWarnings", false),
        ("ignoreUpdateFieldsCheckWarnings", false),
        ("ignoreCheckExternalCallerWarnings", false),
        ("errorOnWarnings", true)
      )
    }
  }

}
