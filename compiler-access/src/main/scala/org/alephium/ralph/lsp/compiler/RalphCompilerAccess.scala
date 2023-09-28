package org.alephium.ralph.lsp.compiler

import fastparse.Parsed
import org.alephium.api.model.CompileProjectResult
import org.alephium.ralph._
import org.alephium.ralph.error.CompilerError.{FastParseError, FormattableError}
import org.alephium.ralph.lsp.compiler.error.StringError
import org.alephium.ralphc.{Config, MetaInfo, Compiler => RalphC}

import java.net.URI
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Using}
import scala.util.matching.Regex

/**
 * Implements ralph parsing and compilation functions accessing the `ralphc`.
 *
 * @note Access to this object is private.
 *       PresentationCompiler does not directly accesses this code.
 */

private object RalphCompilerAccess extends CompilerAccess {

  override def sourceExists(fileURI: URI): Either[FormattableError, Boolean] =
    try
      Right(Files.exists(Paths.get(fileURI)))
    catch {
      case throwable: Throwable =>
        Left(StringError(throwable.getMessage))
    }

  def getSourceFiles(workspaceURI: URI): Either[FormattableError, Seq[URI]] =
    try {
      val uris =
        RalphC
          .getSourceFiles(
            path = Paths.get(workspaceURI),
            ext = s".${CompilerAccess.RALPH_FILE_EXTENSION}"
          ).map(_.toUri)

      Right(uris)
    } catch catchAllThrows

  override def getSourceCode(fileURI: URI): Either[FormattableError, String] =
    Using(Source.fromFile(fileURI))(_.mkString) match {
      case Failure(exception) =>
        catchAllThrows(exception)

      case Success(code) =>
        Right(code)
    }

  private val importRegex = """\s*import\s+"([^".\/]+\/[^"]*[a-z][a-z_0-9])"""".r

  //TODO It would be nice if we were returning a List of Formattable Error
  //for invalid/not found imports
  def handleStdImports(initialCode: String): String = {
    importRegex.findAllMatchIn(initialCode)
      .foldLeft(initialCode) { case (code, pattern) =>
        val importValue = {
          val group =   pattern.group(1)
          if(group.endsWith(".ral")) {
            group.dropRight(4)
          } else {
            group
          }
        }
        StdInterface.stdInterfaces.get(importValue) match {
          case Some(interfaceCode) =>
            code.replaceFirst(pattern.matched, s"\n$interfaceCode\n")
          case None =>
            code
        }
      }
  }

  def parseContracts(code: String): Either[FormattableError, Seq[Ast.ContractWithState]] = {

    val codeWithStdImports = handleStdImports(code)

    try
      fastparse.parse(codeWithStdImports, StatefulParser.multiContract(_)) match {
        case Parsed.Success(ast: Ast.MultiContract, _) =>
          Right(ast.contracts)

        case failure: Parsed.Failure =>
          Left(FastParseError(failure))
      }
    catch catchAllThrows
  }

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
          Left(StringError(error.message))

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
      Left(StringError(error.message))

    case error: Throwable =>
      // TODO: log this to console.
      Left(StringError(error.getMessage))
  }

}
