package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.lsp.compiler.CompilerAccess

import java.net.URI
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

/**
 * Implements functions operating on source-code within a single file.
 */
private[pc] object SourceCode {

  /** Collects paths of all ralph files on disk */
  def initialise(workspaceURI: URI)(implicit compiler: CompilerAccess): Either[CompilerError.FormattableError, ArraySeq[SourceCodeState.OnDisk]] =
    compiler
      .getSourceFiles(workspaceURI)
      .map(_.map(SourceCodeState.OnDisk).to(ArraySeq))

  /**
   * Parse a source file, given its current sate.
   *
   * @param sourceState Current state of the source code
   * @param compiler    Target compiler
   * @return New source code state
   */
  @tailrec
  def parse(sourceState: SourceCodeState)(implicit compiler: CompilerAccess): SourceCodeState =
    sourceState match {
      case SourceCodeState.UnCompiled(fileURI, code) =>
        compiler.parseContracts(code) match {
          case Left(error) =>
            SourceCodeState.ErrorSource(
              fileURI = fileURI,
              code = code,
              errors = Array(error),
              previous = None
            )

          case Right(parsedCode) =>
            SourceCodeState.Parsed(
              fileURI = fileURI,
              code = code,
              contracts = parsedCode,
            )
        }

      case onDisk: SourceCodeState.OnDisk =>
        getSourceCode(onDisk.fileURI) match {
          case errored: SourceCodeState.FailedState =>
            errored

          case gotCode =>
            parse(gotCode)
        }

      case accessError: SourceCodeState.ErrorAccess =>
        // access the code from disk and parse it.
        getSourceCode(accessError.fileURI) match {
          case state: SourceCodeState.UnCompiled =>
            // successfully accessed the code, now parse it.
            parse(state)

          case failed: SourceCodeState.ErrorAccess =>
            // Failed again: Return the error so the client gets reported.
            failed
        }

      case parsed @ (_: SourceCodeState.Parsed | _: SourceCodeState.Compiled) =>
        parsed // code is already in parsed state, return the same state

      case error: SourceCodeState.ErrorSource =>
        // Code was already parsed and it errored.
        // Return the same state.
        error
    }

  private def getSourceCode(fileURI: URI)(implicit compiler: CompilerAccess): SourceCodeState.AccessedState =
    compiler.getSourceCode(fileURI) match {
      case Left(error) =>
        SourceCodeState.ErrorAccess(fileURI, error)

      case Right(sourceCode) =>
        SourceCodeState.UnCompiled(fileURI, sourceCode)
    }

}
