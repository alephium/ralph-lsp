package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.{CompiledContract, CompiledScript}
import org.alephium.ralph.lsp.pc.config.GenCommon._
import org.scalacheck.Gen

import java.net.URI

/**
 * Implements generators for [[SourceCode]] & [[SourceCodeState]] related data-types.
 */
object GenSourceCode {

  def genCompiledCode(): Gen[Seq[Either[CompiledContract, CompiledScript]]] =
    Gen.const(Seq.empty) // TODO: generate these

  def genOnDisk(): Gen[SourceCodeState.OnDisk] =
    for {
      fileURI <- genFile()
    } yield
      SourceCodeState.OnDisk(
        fileURI = fileURI
      )

  def genUnCompiled(code: Gen[String] = genCode): Gen[SourceCodeState.UnCompiled] =
    for {
      fileURI <- genFile()
      code <- code
    } yield
      SourceCodeState.UnCompiled(
        fileURI = fileURI,
        code = code
      )

  def genCompiled(code: Gen[String] = genCode): Gen[SourceCodeState.Compiled] =
    for {
      fileURI <- genFile()
      code <- code
      compiledCode <- genCompiledCode()
      parsedState <- genParsed()
    } yield
      SourceCodeState.Compiled(
        fileURI = fileURI,
        code = code,
        compiledCode = compiledCode,
        previousState = parsedState
      )

  def genParsed(code: Gen[String] = genCode): Gen[SourceCodeState.Parsed] =
    for {
      fileURI <- genFile()
      code <- code
    } yield
      SourceCodeState.Parsed(
        fileURI = fileURI,
        code = code,
        contracts = Seq.empty // TODO: generate these
      )

  def genErrored(code: Gen[String] = genCode): Gen[SourceCodeState.Errored] =
    for {
      fileURI <- genFile()
      code <- code
      errors <- genFormattableErrors(code)
      parsed <- Gen.option(genParsed(Gen.const(code)))
    } yield
      SourceCodeState.Errored(
        fileURI = fileURI,
        code = code,
        errors = errors,
        previous = parsed
      )

  /** Failed access state only */
  def genFailedAccess(fileURI: Gen[URI] = genFile()): Gen[SourceCodeState.FailedAccess] =
    for {
      fileURI <- fileURI
      exceptionMessage <- Gen.alphaStr
    } yield
      SourceCodeState.FailedAccess(
        fileURI = fileURI,
        exception = new Exception(exceptionMessage)
      )

  /** Either one of the failed source-code states */
  def genFailed(code: Gen[String] = genCode): Gen[SourceCodeState.FailedState] =
    Gen.oneOf(
      genErrored(code),
      genFailedAccess()
    )

  /** Generate any one of all source-code states */
  def genSourceCode(): Gen[SourceCodeState] =
    Gen.oneOf(
      genOnDisk(),
      genUnCompiled(),
      genFailed(),
      genParsed(),
      genCompiled(),
    )

}
