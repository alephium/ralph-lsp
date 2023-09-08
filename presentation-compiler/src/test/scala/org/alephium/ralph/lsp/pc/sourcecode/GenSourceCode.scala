package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.pc.config.GenCommon._
import org.scalacheck.Gen

import java.net.URI

/**
 * Implements generators for [[SourceCode]] & [[SourceCodeState]] related data-types.
 */
object GenSourceCode {

  def genParsedSourceCode(code: Gen[String] = genSourceCode): Gen[SourceCodeState.Parsed] =
    for {
      uri <- genFileNameURI()
      code <- code
    } yield
      SourceCodeState.Parsed(
        fileURI = uri,
        code = code,
        contracts = Seq.empty // TODO: generate these
      )

  def genErroredSourceCode(code: Gen[String] = genSourceCode): Gen[SourceCodeState.Errored] =
    for {
      uri <- genFileNameURI()
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

  /** Failed access state only */
  def genFailedAccessSourceCode(uri: Gen[URI] = genFileNameURI()): Gen[SourceCodeState.FailedAccess] =
    for {
      uri <- uri
      exceptionMessage <- Gen.alphaStr
    } yield
      SourceCodeState.FailedAccess(
        fileURI = uri,
        exception = new Exception(exceptionMessage)
      )

  /** Either one of the failed source-code states */
  def genFailedSourceCodeState(code: Gen[String] = genSourceCode): Gen[SourceCodeState.FailedState] =
    Gen.oneOf(
      genErroredSourceCode(code),
      genFailedAccessSourceCode()
    )

}
