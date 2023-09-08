package org.alephium.ralph.lsp.compiler

import fastparse.Parsed
import org.alephium.api.model.CompileProjectResult
import org.alephium.ralph._
import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.error.CompilerError.{FastParseError, FormattableError}
import org.alephium.ralph.lsp.compiler.error.WorkspaceError
import org.alephium.ralphc.{Config, MetaInfo, Compiler => RalphC}

import java.net.URI
import java.nio.file.Path
import scala.collection.mutable
import scala.io.Source
import scala.util.{Try, Using}

/**
 * Implements ralph parsing and compilation functions accessing the ralph compiler code.
 *
 * @note Access to this object is private.
 *       PresentationCompiler does not directly accesses this code.
 */

private object RalphCompilerAccess extends CompilerAccess {

  def getSourceFiles(workspaceURI: Path): Try[Seq[Path]] =
    Try(RalphC.getSourceFiles(workspaceURI, s".${CompilerAccess.RALPH_FILE_EXTENSION}"))

  override def getSourceCode(fileURI: URI): Try[String] =
    Using(Source.fromFile(fileURI))(_.mkString)

  def parseContracts(code: String): Either[CompilerError.FormattableError, Seq[Ast.ContractWithState]] =
    try
      fastparse.parse(code, StatefulParser.multiContract(_)) match {
        case Parsed.Success(ast: Ast.MultiContract, _) =>
          Right(ast.contracts)

        case failure: Parsed.Failure =>
          Left(FastParseError(failure))
      }
    catch catchAllThrows

  def compileContracts(contracts: Seq[Ast.ContractWithState],
                       options: CompilerOptions): Either[FormattableError, (Array[CompiledContract], Array[CompiledScript])] =
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
                                    config: Config): Either[FormattableError, (Array[CompiledContract], Array[CompiledScript])] =
    try {
      val ralphc = RalphC(config)
      ralphc.compileProject() match {
        case Left(error) =>
          Left(WorkspaceError(error))

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

  private def catchAllThrows[T]: PartialFunction[Throwable, Either[FormattableError, T]] = {
    case error: FormattableError =>
      Left(error)

    case error: org.alephium.ralph.Compiler.Error =>
      Left(WorkspaceError(error))

    case error: Throwable =>
      Left(WorkspaceError(error))
  }

}
