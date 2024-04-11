package org.alephium.ralph.lsp.access.compiler

import fastparse.Parsed
import org.alephium.ralph._
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.compiler.message.error._
import org.alephium.ralph.lsp.access.util.TryUtil

import java.net.URI

/**
 * Implements ralph parsing and compilation functions accessing the `ralphc`.
 *
 * @note Access to this object is private.
 *       PresentationCompiler does not directly accesses this code.
 */

private object RalphCompilerAccess extends CompilerAccess {

  /** @inheritdoc */
  def parseContracts(
      fileURI: URI,
      code: String): Either[CompilerMessage.AnyError, Tree.Root] =
    try
      fastparse.parse(code, RalphParserExtension.multiContract(fileURI)(_)) match {
        case Parsed.Success(source: Tree.Root, _) =>
          Right(source)

        case failure: Parsed.Failure =>
          Left(FastParseError(failure))
      }
    catch TryUtil.catchAllThrows(fileURI)

  /** @inheritdoc */
  def compileContracts(
      contracts: Seq[Ast.ContractWithState],
      structs: Seq[Ast.Struct],
      options: CompilerOptions,
      workspaceErrorURI: URI): Either[CompilerMessage.AnyError, (Array[CompiledContract], Array[CompiledScript])] =
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
    } catch TryUtil.catchAllThrows(workspaceErrorURI)

}
