package org.alephium.ralph.lsp.access.compiler

import fastparse.Parsed
import org.alephium.api.model.CompileProjectResult
import org.alephium.ralph._
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.error._
import org.alephium.ralph.lsp.access.util.TryUtil
import org.alephium.ralphc.{Config, MetaInfo, Compiler => RalphC}

import java.net.URI
import scala.collection.mutable

/**
 * Implements ralph parsing and compilation functions accessing the `ralphc`.
 *
 * @note Access to this object is private.
 *       PresentationCompiler does not directly accesses this code.
 */

private object RalphCompilerAccess extends CompilerAccess {

  /** @inheritdoc */
  def parseContracts(code: String): Either[CompilerMessage.AnyError, Tree.Root] =
    try
      fastparse.parse(code, RalphParserExtension.multiContract(_)) match {
        case Parsed.Success(source: Tree.Root, _) =>
          Right(source)

        case failure: Parsed.Failure =>
          Left(FastParseError(failure))
      }
    catch TryUtil.catchAllThrows

  /** @inheritdoc */
  def compileContracts(contracts: Seq[Ast.ContractWithState],
                       structs: Seq[Ast.Struct],
                       options: CompilerOptions): Either[CompilerMessage.AnyError, (Array[CompiledContract], Array[CompiledScript])] =
    try {
      val multiContract =
        Ast.MultiContract(contracts, structs, None)

      val extendedContracts =
        multiContract.extendedContracts()

      val statefulContracts =
        extendedContracts
          .genStatefulContracts()(options)
          .map(_._1)

      val statefulScripts =
        extendedContracts.genStatefulScripts()(options)

      Right((statefulContracts.toArray, statefulScripts.toArray))
    } catch TryUtil.catchAllThrows

  /** @inheritdoc */
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
    } catch TryUtil.catchAllThrows

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

}
