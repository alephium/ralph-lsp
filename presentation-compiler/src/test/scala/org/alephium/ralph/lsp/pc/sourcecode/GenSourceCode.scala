package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.{CompiledContract, CompiledScript}
import org.alephium.ralph.lsp.compiler.error.StringMessage
import org.alephium.ralph.Ast.ContractWithState
import org.alephium.ralph.lsp.GenCommon._
import org.scalacheck.Gen

import java.net.URI
import scala.util.Random

/**
 * Implements generators for [[SourceCode]] & [[SourceCodeState]] related data-types.
 */
object GenSourceCode {

  def genCompiledCode(): Gen[Seq[Either[CompiledContract, CompiledScript]]] =
    Gen.const(Seq.empty) // TODO: generate these

  def genParsedContracts(): Gen[Seq[ContractWithState]] =
    Gen.const(Seq.empty) // TODO: generate these

  def genOnDisk(): Gen[SourceCodeState.OnDisk] =
    for {
      fileURI <- genFileURI()
    } yield
      SourceCodeState.OnDisk(
        fileURI = fileURI
      )

  def genUnCompiled(code: Gen[String] = genCode): Gen[SourceCodeState.UnCompiled] =
    for {
      fileURI <- genFileURI()
      code <- code
    } yield
      SourceCodeState.UnCompiled(
        fileURI = fileURI,
        code = code
      )

  def genCompiled(code: Gen[String] = genCode): Gen[SourceCodeState.Compiled] =
    for {
      fileURI <- genFileURI()
      code <- code
      compiledCode <- genCompiledCode()
      parsedState <- genParsed()
    } yield
      SourceCodeState.Compiled(
        fileURI = fileURI,
        code = code,
        compiledCode = compiledCode,
        parsed = parsedState
      )

  def genParsed(code: Gen[String] = genCode): Gen[SourceCodeState.Parsed] =
    for {
      fileURI <- genFileURI()
      code <- code
    } yield
      SourceCodeState.Parsed(
        fileURI = fileURI,
        code = code,
        contracts = Seq.empty // TODO: generate these
      )

  def genParsedOrCompiled(code: Gen[String] = genCode): Gen[SourceCodeState.ParsedState] =
    Gen.oneOf(
      genParsed(code),
      genCompiled(code)
    )

  def genErrorSource(code: Gen[String] = genCode): Gen[SourceCodeState.ErrorSource] =
    for {
      fileURI <- genFileURI()
      code <- code
      errors <- genErrors(code)
      parsed <- Gen.option(genParsed(Gen.const(code)))
    } yield
      SourceCodeState.ErrorSource(
        fileURI = fileURI,
        code = code,
        errors = errors,
        previous = parsed
      )

  /** Failed access state only */
  def genFailedAccess(fileURI: Gen[URI] = genFileURI()): Gen[SourceCodeState.ErrorAccess] =
    for {
      fileURI <- fileURI
      errorMessage <- Gen.alphaStr
    } yield
      SourceCodeState.ErrorAccess(
        fileURI = fileURI,
        error = StringMessage(errorMessage) // TODO: Call a generator
      )

  /** Either one of the failed source-code states */
  def genFailed(code: Gen[String] = genCode): Gen[SourceCodeState.FailedState] =
    Gen.oneOf(
      genErrorSource(code),
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

  /** Must contain at least on errored SourceCode in the Workspace */
  def genAtLeastOneFailed(): Gen[List[SourceCodeState]] =
    for {
      sourceCodes <- Gen.listOf(genSourceCode())
      erroredCode <- Gen.nonEmptyListOf(GenSourceCode.genFailed())
    } yield Random.shuffle(sourceCodes ++ erroredCode)

  def genParsedOrCompiledWithAtLeastOneFailed(): Gen[List[SourceCodeState]] =
    for {
      goodCode <- Gen.listOf(genParsedOrCompiled())
      erroredCode <- Gen.nonEmptyListOf(GenSourceCode.genFailed())
    } yield Random.shuffle(goodCode ++ erroredCode)
}
