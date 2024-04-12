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

package org.alephium.ralph.lsp.access.compiler.message.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.TestCode
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.scalacheck.Gen

/** [[CompilerMessage]] related test functions */
object TestError {

  /** Generate an error for this code */
  def genError(code: Gen[String] = TestCode.genGoodCode()): Gen[CompilerMessage.AnyError] =
    for {
      code         <- code
      errorMessage <- Gen.alphaStr
      errorIndex   <- Gen.choose(0, code.length - 1)
    } yield StringError(
      message = errorMessage,
      index = SourceIndex(0, errorIndex, None) // TODO: gen random index location
    )

  def genErrors(code: String): Gen[List[CompilerMessage.AnyError]] =
    Gen.listOf(genError(Gen.const(code)))

}
