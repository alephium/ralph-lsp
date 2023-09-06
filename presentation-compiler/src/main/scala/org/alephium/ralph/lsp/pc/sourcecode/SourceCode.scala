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

  def initialise(path: Path): Try[ArraySeq[SourceCodeState.OnDisk]] =
    FileIO
      .getFiles(path, s".${PCConfig.RALPH_FILE_EXTENSION}")
      .map(_.map(SourceCodeState.OnDisk).to(ArraySeq))

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

      case parsed: SourceCodeState.Parsed =>
        parsed

      case compiled: SourceCodeState.Compiled =>
        compiled

      case state: SourceCodeState.FailedState =>
        getSourceCode(state.fileURI) match {
          case state: SourceCodeState.UnCompiled =>
            parse(state)

          case failed: SourceCodeState.FailedAccess =>
            // maybe file does not exists.
            // this will case this state to fail compilation and this error will get reported.
            failed
        }
    }

  private def getSourceCode(fileURI: URI): SourceCodeState.ReadState =
    FileIO.readAllLines(fileURI) match {
      case Failure(exception) =>
        SourceCodeState.FailedAccess(fileURI, exception)

      case Success(sourceCode) =>
        SourceCodeState.UnCompiled(fileURI, sourceCode)
    }

}
