package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.{CompiledContract, CompiledScript}
import org.alephium.ralph.lsp.compiler.error.FileError
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
        previousState = parsedState
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

  def genErrored(code: Gen[String] = genCode): Gen[SourceCodeState.Errored] =
    for {
      fileURI <- genFileURI()
      code <- code
      errors <- genErrors(code)
      parsed <- Gen.option(genParsed(Gen.const(code)))
    } yield
      SourceCodeState.Errored(
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
        error = FileError(errorMessage) // TODO: Call a generator
      )

  /** Either one of the failed source-code states */
  def genFailed(code: Gen[String] = genCode): Gen[SourceCodeState.ErrorState] =
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

  /** Must contain at least on errored SourceCode in the Workspace */
  def genAtLeastOneError(): Gen[List[SourceCodeState]] =
    for {
      sourceCodes <- Gen.listOf(genSourceCode())
      erroredCode <- GenSourceCode.genErrored()
    } yield Random.shuffle(sourceCodes :+ erroredCode)
}
