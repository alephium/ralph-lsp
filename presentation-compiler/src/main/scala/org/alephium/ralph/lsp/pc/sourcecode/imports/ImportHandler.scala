package org.alephium.ralph.lsp.pc.sourcecode.imports

import fastparse._
import fastparse.Parsed
import scala.collection.immutable.ArraySeq

import org.alephium.ralph.Ast.ContractWithState
import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.compiler.message.{CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.compiler.message.error.{ImportError, FastParseError}
import org.alephium.ralph.lsp.pc.workspace.build.BuildDependencies
import org.alephium.ralph

object ImportHandler {

  case class ParseResult(code: String, parsedImports: Seq[ParsedImport])

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

  private def findAndReplaceImports(initialCode: String, parsedImports:Seq[ParsedImport]) = {
    parsedImports.foldLeft(initialCode) {
      case (code, parsedImport) =>
        replaceImport(code, parsedImport)
    }
  }

  private def replaceImport(code:String, parsedImport: ParsedImport): String = {
    //We preserve same size as initial parsing
    val patch = parsedImport.fullParse.replaceFirst("import", s"//mprt").replaceFirst(s""""${parsedImport.name}"""", s"//${parsedImport.name}")
    code.patch(parsedImport.fullParseIndex, patch, patch.size)
  }

  def compileImports(imports: Seq[ParsedImport])(implicit compiler: CompilerAccess, buildDependencies: BuildDependencies): Seq[Either[CompilerMessage.AnyError, Seq[ContractWithState]]] = {
    imports.map { parsedImport =>
      for {
        code <- validateImportAndGetCode(parsedImport)
        compiled <- compiler.parseContracts(code)
       } yield compiled
     }
  }

  private def validateImportAndGetCode(parsedImport: ParsedImport)(implicit buildDependencies: BuildDependencies): Either[CompilerMessage.AnyError, String] =
    buildDependencies.stdInterfaces.get(parsedImport.name.value).toRight {
      ImportError.Unknown(parsedImport.name.value, SourceIndex(parsedImport.index, parsedImport.name.value.size))
  }
}
