package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.compiler.CompilerAccess

import java.net.URI
import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.util.{Failure, Success, Try}

private[pc] object SourceCode {

  /** Collects paths of all ralph files on disk */
  def initialise(path: Path)(implicit compiler: CompilerAccess): Try[ArraySeq[SourceCodeState.OnDisk]] =
    compiler
      .getSourceFiles(path)
      .map {
        _.map {
          path =>
            SourceCodeState.OnDisk(path.toUri)
        }.to(ArraySeq)
      }

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
            SourceCodeState.Errored(
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

      case parsed @ (_: SourceCodeState.Parsed | _: SourceCodeState.Compiled) =>
        parsed // code is already in parsed state, return the same state

      case state: SourceCodeState.FailedState =>
        // access the code from disk and parse it.
        getSourceCode(state.fileURI) match {
          case state: SourceCodeState.UnCompiled =>
            // successfully accessed the code, now parse it.
            parse(state)

          case failed: SourceCodeState.FailedAccess =>
            // Failed again: Maybe file does not exists. Return the error so the client gets reported.
            failed
        }
    }

  private def getSourceCode(fileURI: URI)(implicit compiler: CompilerAccess): SourceCodeState.AccessState =
    compiler.getSourceCode(fileURI) match {
      case Failure(exception) =>
        SourceCodeState.FailedAccess(fileURI, exception)

      case Success(sourceCode) =>
        SourceCodeState.UnCompiled(fileURI, sourceCode)
    }

}
