package org.alephium.ralph.lsp.pc.sourcecode

import fastparse._
import fastparse.Parsed
import org.alephium.ralph.lsp.compiler.message.{CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.compiler.message.error.{ImportError, FastParseError}
import org.alephium.ralph

object ImportHandler {

  case class Result(code: String, imports: Map[String, String])

  /*
   * Parse source code and find all imports.
   * Imports are resolved with their corresponding file, if not found we return an error.
   * We pass through all imports so we can have the full list of non-existing import
   */
  def extractStdImports(code: String): Either[Seq[CompilerMessage.AnyError], Result] = {
    fastparse.parse(code, ImportLexer.imports(_)) match {
      case Parsed.Success(parsedImports, index) =>
        val (finalCode, finalImports, errors) = findAndReplaceImports(code, parsedImports )

        if(errors.nonEmpty) {
          Left(errors)
        } else {
          Right(Result(finalCode, finalImports))
        }
      case failure:Parsed.Failure =>
        Left(Seq(FastParseError(failure)))
    }
  }

  private def findAndReplaceImports(initialCode: String, parsedImports:Seq[ParsedImport] ) = {
    parsedImports.foldLeft((initialCode, Map.empty[String,String], Seq.empty): (String, Map[String,String], Seq[CompilerMessage.AnyError])) {
      case ((code, imports, errors), parsedImport) =>

        validateAndReplaceImport(code, parsedImport) match {
          case Right((nextCode, newImport)) =>
            (nextCode, imports ++ newImport, errors)
          case Left((nextCode, error)) =>
            (nextCode, imports, errors :+ error)
        }
    }

  }

  private def replaceImport(code:String, parsedImport: ParsedImport): String = {
    //We preserve same size as initial parsing
    val patch = parsedImport.fullParse.replaceFirst("import", s"//mprt").replaceFirst(s""""${parsedImport.name}"""", s"//${parsedImport.name}")
    code.patch(parsedImport.fullParseIndex, patch, patch.size)
  }

  private def validateAndReplaceImport(code:String, parsedImport:ParsedImport):Either[(String, CompilerMessage.AnyError), (String, Map[String,String])] ={
    StdInterface.stdInterfaces.get(parsedImport.name.value) match {
      case Some(interfaceCode) =>
        Right((replaceImport(code, parsedImport), Map(parsedImport.name.value -> interfaceCode)))
      case None =>
        //Wrong imports are still replaced to avoid matching their index on a future import error
        Left((replaceImport(code, parsedImport), ImportError.Unknown(parsedImport.name.value, SourceIndex(parsedImport.index, parsedImport.name.value.size))))
    }
  }
}
