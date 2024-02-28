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
      code <- code
      errorMessage <- Gen.alphaStr
      errorIndex <- Gen.choose(0, code.length - 1)
    } yield
      StringError(
        message = errorMessage,
        index = SourceIndex(0, errorIndex) // TODO: gen random index location
      )

  def genErrors(code: String): Gen[List[CompilerMessage.AnyError]] =
    Gen.listOf(genError(Gen.const(code)))

}
