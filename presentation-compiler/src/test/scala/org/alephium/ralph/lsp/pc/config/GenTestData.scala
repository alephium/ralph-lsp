package org.alephium.ralph.lsp.pc.config

import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.scalacheck.Gen

import java.net.URI

object GenTestData {

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

  def genParsedSourceCode(code: Gen[String] = genSourceCode): Gen[SourceCodeState.Parsed] =
    for {
      uri <- genURI
      code <- code
    } yield
      SourceCodeState.Parsed(
        fileURI = uri,
        code = code,
        contracts = Seq.empty // TODO: generate these
      )

  def genErroredSourceCode(code: Gen[String] = genSourceCode): Gen[SourceCodeState.Errored] =
    for {
      uri <- genURI
      code <- code
      errors <- genFormattableErrors(code)
      parsed <- Gen.option(genParsedSourceCode(Gen.const(code)))
    } yield
      SourceCodeState.Errored(
        fileURI = uri,
        code = code,
        errors = errors,
        previous = parsed
      )

  def genFailedAccessSourceCode(uri: Gen[URI] = genURI): Gen[SourceCodeState.FailedAccess] =
    for {
      uri <- uri
      exceptionMessage <- Gen.alphaStr
    } yield
      SourceCodeState.FailedAccess(
        fileURI = uri,
        exception = new Exception(exceptionMessage)
      )

  def genFailedSourceCodeState(code: Gen[String] = genSourceCode): Gen[SourceCodeState.FailedState] =
    Gen.oneOf(
      genErroredSourceCode(code),
      genFailedAccessSourceCode()
    )

}
