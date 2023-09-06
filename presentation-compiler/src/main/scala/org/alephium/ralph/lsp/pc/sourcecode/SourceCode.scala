package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.pc.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.util.FileIO
import org.alephium.ralph.lsp.pc.PCConfig

import java.net.URI
import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.util.{Failure, Success, Try}

private[pc] object SourceCode {

  /** Collects paths of all ralph files on disk */
  def initialise(path: Path): Try[ArraySeq[SourceCodeState.OnDisk]] =
    FileIO
      .getFiles(path, s".${PCConfig.RALPH_FILE_EXTENSION}")
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
        compiler.parseCode(code) match {
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
              parsedAST = parsedCode,
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

  private def getSourceCode(fileURI: URI): SourceCodeState.AccessState =
    FileIO.readAllLines(fileURI) match {
      case Failure(exception) =>
        SourceCodeState.FailedAccess(fileURI, exception)

      case Success(sourceCode) =>
        SourceCodeState.UnCompiled(fileURI, sourceCode)
    }

}
