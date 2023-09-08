package org.alephium.ralph.lsp.pc.config

import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.error.CompilerError.FormattableError
import org.scalacheck.Gen

import java.net.URI

/**
 * Common test data generator used by all other data types.
 */
object GenCommon {

  val genURI: Gen[URI] =
    Gen.listOfN(10, Gen.alphaChar).map(_.mkString) map {
      fileName =>
        URI.create(s"""$fileName.ral""")
    }

  // TODO: Generate actual sourceCode
  val genSourceCode: Gen[String] =
    Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString)

  /** Generate some error for this code */
  def genFormattableError(forCode: String): Gen[FormattableError] =
    for {
      errorMessage <- Gen.alphaStr
      errorIndex <- Gen.choose(0, forCode.length - 1)
    } yield
      CompilerError.`Invalid number`(errorMessage, errorIndex)

  def genFormattableErrors(forCode: String): Gen[List[FormattableError]] =
    Gen.listOf(genFormattableError(forCode))

}
