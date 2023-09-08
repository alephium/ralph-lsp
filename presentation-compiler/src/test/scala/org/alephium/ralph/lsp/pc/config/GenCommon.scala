package org.alephium.ralph.lsp.pc.config

import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.compiler.CompilerAccess.RALPH_FILE_EXTENSION
import org.scalacheck.Gen

import java.net.URI

/**
 * Common test data generator used by all other data types.
 */
object GenCommon {

  // TODO: Generate actual sourceCode
  val genSourceCode: Gen[String] =
    Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString)

  val genFileName: Gen[String] =
    Gen.listOfN(10, Gen.alphaChar).map(_.mkString)

  /** Generate some error for this code */
  def genFormattableError(forCode: String): Gen[FormattableError] =
    for {
      errorMessage <- Gen.alphaStr
      errorIndex <- Gen.choose(0, forCode.length - 1)
    } yield
      CompilerError.`Invalid number`(errorMessage, errorIndex)

  def genFormattableErrors(forCode: String): Gen[List[FormattableError]] =
    Gen.listOf(genFormattableError(forCode))

  def genFileName(ext: String = RALPH_FILE_EXTENSION): Gen[String] =
    genFileName map {
      name =>
        s"$name.$ext"
    }

  def genFileNameURI(ext: String = RALPH_FILE_EXTENSION): Gen[URI] =
    genFileName(ext) map URI.create

}
