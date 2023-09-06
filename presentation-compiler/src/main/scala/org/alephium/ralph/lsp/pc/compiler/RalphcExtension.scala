package org.alephium.ralph.lsp.pc.compiler

import fastparse._
import org.alephium.api.model.CompileProjectResult
import org.alephium.ralph._
import org.alephium.ralph.error.CompilerError.{FastParseError, FormattableError}
import org.alephium.ralph.lsp.pc.data.WorkspaceError
import org.alephium.ralphc.{Config, MetaInfo, Compiler => RalphC}

import java.net.URI
import scala.collection.mutable

/**
 * Implements functions that are required from Ralphc.
 *
 * @note Access to this object is private.
 *       PresentationCompiler does not directly accesses this code.
 */
private object RalphcExtension {

  def compileMultiContract(multiContract: Ast.MultiContract,
                           compilerOptions: CompilerOptions): Either[FormattableError, (Array[CompiledContract], Array[CompiledScript])] =
    try {
      val extendedContracts =
        multiContract.extendedContracts()

      val statefulContracts =
        extendedContracts
          .genStatefulContracts()(compilerOptions)
          .map(_._1)

      val statefulScripts =
        extendedContracts.genStatefulScripts()(compilerOptions)

      Right((statefulContracts.toArray, statefulScripts.toArray))
    } catch catchAllThrows

  def parseMultiContract(code: String): Either[FormattableError, Ast.MultiContract] =
    try
      fastparse.parse(code, StatefulParser.multiContract(_)) match {
        case Parsed.Success(ast: Ast.MultiContract, _) =>
          Right(ast)

        case failure: Parsed.Failure =>
          Left(FastParseError(failure))
      }
    catch catchAllThrows

  def compileForDeployment(workspaceURI: URI,
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
