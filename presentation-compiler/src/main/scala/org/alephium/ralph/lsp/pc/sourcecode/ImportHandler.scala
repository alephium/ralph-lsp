package org.alephium.ralph.lsp.pc.sourcecode

import fastparse._
import fastparse.Parsed
import fastparse.MultiLineWhitespace._
import org.alephium.ralph.lsp.compiler.message.{CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.compiler.message.error.{ImportError, FastParseError}

object ImportHandler {

  case class Result(code: String, imports: Map[String, String])

  object Lexer {
    def importKey[Unknown:P]: P[Unit] = P("import")

    def interfaceName[Unknown: P]: P[String] = {
      implicit val whitespace: P[_] => P[Unit] = fastparse.NoWhitespace.noWhitespaceImplicit
      P((CharIn("a-z") | "/" | "_" | "-" | ".").rep).!
    }

    def importValue[Unknown: P]: P[String] = {
      implicit val whitespace: P[_] => P[Unit] = fastparse.NoWhitespace.noWhitespaceImplicit
      P("\"" ~ interfaceName ~ "\"")
    }

    def stdInterface[Unknown:P]: P[String] = P(importKey ~ importValue )

    def imports[Unknown:P]: P[Seq[String]] = P(Start ~ stdInterface.rep)
  }


  /*
   * Parse source code and find all imports.
   * Imports are resolved with their corresponding file, if not found we return an error.
   * We pass through all imports so we can have the full list of non-existing import
   */
  def extractStdImports(code: String): Either[Seq[CompilerMessage.AnyError], Result] = {
    fastparse.parse(code, Lexer.imports(_)) match {
      case Parsed.Success((parsedImports), index) =>
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

  private def findAndReplaceImports(initialCode: String, parsedImports:Seq[String] ) = {
    parsedImports.foldLeft((initialCode, Map.empty[String,String], Seq.empty): (String, Map[String,String], Seq[CompilerMessage.AnyError])) {
      case ((code, imports, errors), rawImport) =>

        val importValue = cleanImportValue(rawImport)

        validateAndReplaceImport(code, importValue) match {
          case Right((nextCode, newImport)) =>
            (nextCode, imports ++ newImport, errors)
          case Left((nextCode, error)) =>
            (nextCode, imports, errors :+ error)
        }
    }
  }

  private def cleanImportValue(value: String): String = {
    if(value.endsWith(".ral")) {
      value.dropRight(4)
    } else {
      value
    }
  }

  private def replaceImport(code:String, importValue:String): String = {
    code.replaceFirst("import", "//    ").replaceFirst(s""""$importValue"""",s"//$importValue")
  }

  private def validateAndReplaceImport(code:String, importValue:String):Either[(String, CompilerMessage.AnyError), (String, Map[String,String])] ={
    StdInterface.stdInterfaces.get(importValue) match {
      case Some(interfaceCode) =>
        Right((replaceImport(code, importValue), Map(importValue-> interfaceCode)))
      case None =>
        val index = code.indexOf(s""""$importValue""") + 1
        //Wrong imports are still replaced to avoid matching their index on a future import error
        Left((replaceImport(code, importValue), ImportError.Unknown(importValue, SourceIndex(index, importValue.size))))
    }
  }
}
