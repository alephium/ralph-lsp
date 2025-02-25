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
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST.Space
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

class FunctionBlockSpec extends AnyWordSpec with Matchers {

  "function name is not defined" when {
    "block is defined" when {
      "block is empty" in {
        val function = parseFunction("fn -> {}")
        // the block '{}' is parsed even though a lot of the function signature is missing, e.g., function name.
        val block = function.block.value

        block.index shouldBe indexOf("fn -> >>{}<<")
        block.parts shouldBe empty
      }

      "block contains a space" in {
        val function = parseFunction("fn -> { }")
        // the block '{ }' is parsed even though a lot of the function signature is missing.
        val block = function.block.value

        block.index shouldBe indexOf("fn -> >>{ }<<")

        block.parts should contain only Space("fn -> {>> <<}")
      }

      "closing curly is missing" in {
        val function =
          parseFunction("fn -> {")

        // even with error syntax, the block still gets parsed, reporting the missing closing curly brace
        val block = function.block.value
        block.index shouldBe indexOf("fn -> >>{<<")

        block.parts shouldBe empty

        block.closeCurly shouldBe
          SoftAST.TokenExpected(indexOf("fn -> {>><<"), Token.CloseCurly)
      }
    }
  }

}
