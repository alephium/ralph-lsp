package org.alephium.ralph.lsp.compiler

import fastparse.Parsed
import org.alephium.api.model.CompileProjectResult
import org.alephium.ralph._
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.compiler.message.error.{FastParseError, FormattedError, StringError}
import org.alephium.ralphc.{Config, MetaInfo, Compiler => RalphC}

import java.net.URI
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Using}

/**
 * Implements ralph parsing and compilation functions accessing the `ralphc`.
 *
 * @note Access to this object is private.
 *       PresentationCompiler does not directly accesses this code.
 */

private object RalphCompilerAccess extends CompilerAccess {

  override def sourceExists(fileURI: URI): Either[CompilerMessage.AnyError, Boolean] =
    try
      Right(Files.exists(Paths.get(fileURI)))
    catch {
      case throwable: Throwable =>
        Left(StringError(throwable))
    }

  def getSourceFiles(workspaceURI: URI): Either[CompilerMessage.AnyError, Seq[URI]] =
    try {
      val uris =
        RalphC
          .getSourceFiles(
            path = Paths.get(workspaceURI),
            ext = s".${CompilerAccess.RALPH_FILE_EXTENSION}"
          ).map(_.toUri)

      Right(uris)
    } catch catchAllThrows

  override def getSourceCode(fileURI: URI): Either[CompilerMessage.AnyError, String] =
    Using(Source.fromFile(fileURI))(_.mkString) match {
      case Failure(exception) =>
        catchAllThrows(exception)

      case Success(code) =>
        Right(code)
    }

  def parseContracts(code: String): Either[CompilerMessage.AnyError, Seq[Ast.ContractWithState]] =
    try
      fastparse.parse(code, StatefulParser.multiContract(_)) match {
        case Parsed.Success(ast: Ast.MultiContract, _) =>
          Right(ast.contracts)

        case failure: Parsed.Failure =>
          Left(FastParseError(failure))
      }
    catch catchAllThrows

  def compileContracts(contracts: Seq[Ast.ContractWithState],
                       options: CompilerOptions): Either[CompilerMessage.AnyError, (Array[CompiledContract], Array[CompiledScript])] =
    try {
      val multiContract =
        Ast.MultiContract(contracts, None)

      val extendedContracts =
        multiContract.extendedContracts()

      val statefulContracts =
        extendedContracts
          .genStatefulContracts()(options)
          .map(_._1)

      val statefulScripts =
        extendedContracts.genStatefulScripts()(options)

      Right((statefulContracts.toArray, statefulScripts.toArray))
    } catch catchAllThrows

  override def compileForDeployment(workspaceURI: URI,
                                    config: Config): Either[CompilerMessage.AnyError, (Array[CompiledContract], Array[CompiledScript])] =
    try {
      val ralphc = RalphC(config)
      ralphc.compileProject() match {
        case Left(error) =>
          Left(StringError(error))

        case Right(result) =>
          Right(buildSuccessfulCompilation(result, ralphc.metaInfos))
      }
    } catch catchAllThrows

  private def buildSuccessfulCompilation(result: CompileProjectResult,
                                         metaInfos: mutable.Map[String, MetaInfo]): (Array[CompiledContract], Array[CompiledScript]) = {
    val scripts: Array[CompiledScript] = ???
    //      result.scripts map {
    //        script =>
    //          val metaInfo = metaInfos(script.name)
    //          val fileURI = getFileURI(metaInfo)
    //          ???
    //      }

    val contracts: Array[CompiledContract] = ???
    //      result.contracts map {
    //        contract =>
    //          val metaInfo = metaInfos(contract.name)
    //          val fileURI = getFileURI(metaInfo)
    //          ???
    //      }

    (contracts, scripts)
  }

  /** Given the MetaInfo, fetch the file URI */
  private def getFileURI(metaInfo: MetaInfo): URI =
    metaInfo
      .artifactPath
      .getParent
      .resolve(s"${metaInfo.name}.ral")
      .toUri

  private def catchAllThrows[T]: PartialFunction[Throwable, Either[CompilerMessage.AnyError, T]] = {
    case error: FormattableError =>
      Left(FormattedError(error))

    case error: org.alephium.ralph.Compiler.Error =>
      Left(StringError(error.message))

    case error: Throwable =>
      // TODO: log this to console.
      Left(StringError(error.getMessage))
  }

}
