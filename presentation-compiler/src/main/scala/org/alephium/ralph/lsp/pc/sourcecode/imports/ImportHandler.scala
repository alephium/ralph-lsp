package org.alephium.ralph.lsp.pc.sourcecode.imports

import fastparse._
import fastparse.Parsed

import org.alephium.ralph.Ast.ContractWithState
import org.alephium.ralph.lsp.pc.workspace.build.BuildDependencies
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.access.compiler.message.error.{ImportError, FastParseError}
import org.alephium.ralph

object ImportHandler {

  case class ParseResult(code: String, parsedImports: Seq[ImportState.Parsed])

  /*
   * Parse source code and find all imports.
   * Imports are simply parsed, not validated yet
   */
  def parseImports(code: String): Either[Seq[CompilerMessage.AnyError], ParseResult] = {
    fastparse.parse(code, ImportLexer.imports(_)) match {
      case Parsed.Success(parsedImports, index) =>
        val finalCode = findAndReplaceImports(code, parsedImports)
        Right(ParseResult(finalCode, parsedImports))
      case failure:Parsed.Failure =>
        Left(Seq(FastParseError(failure)))
    }
  }

  private def findAndReplaceImports(initialCode: String, parsedImports:Seq[ImportState.Parsed]) = {
    parsedImports.foldLeft(initialCode) {
      case (code, parsedImport) =>
        replaceImport(code, parsedImport)
    }
  }

  private def replaceImport(code:String, parsedImport: ImportState.Parsed): String = {
    //We preserve same size as initial parsing for source indexing
    //but erase everything so parsing can work
    val patch = parsedImport.fullParse.map{
      case '\n' => '\n'
      case _ => ' '
    }
    code.patch(parsedImport.fullParseIndex, patch, parsedImport.fullParse.size)
  }

  def compileImports(imports: Seq[ImportState.Parsed])(implicit compiler: CompilerAccess, buildDependencies: BuildDependencies): Seq[Either[CompilerMessage.AnyError, ImportState.Compiled]] = {
    imports.map { parsedImport =>
      for {
        code <- validateImportAndGetCode(parsedImport)
        compiled <- compiler.parseContracts(code)
       } yield ImportState.Compiled(parsedImport, compiled)
     }
  }

  private def validateImportAndGetCode(parsedImport: ImportState.Parsed)(implicit buildDependencies: BuildDependencies): Either[CompilerMessage.AnyError, String] =
    buildDependencies.stdInterfaces.get(parsedImport.name.value).toRight {
      ImportError.Unknown(parsedImport.name.value, SourceIndex(parsedImport.index, parsedImport.name.value.size))
  }
}
