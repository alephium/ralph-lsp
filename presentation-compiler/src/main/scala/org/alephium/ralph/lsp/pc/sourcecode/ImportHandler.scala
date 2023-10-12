package org.alephium.ralph.lsp.pc.sourcecode

import fastparse._
import fastparse.Parsed
import org.alephium.ralph.lsp.compiler.message.{CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.compiler.message.error.{ImportError, FastParseError}
import org.alephium.ralph

object ImportHandler {

  case class Result(code: String, imports: Map[String, String])

  case class ImportName private(value: String) extends AnyVal {
    override def toString:String = value
  }
  object ImportName {
    def cleaned(value:String): ImportName = {
      //Remove start and end quote if exist
      val tmp = value.replaceAll("^\"|\"$", "");
      if(tmp.endsWith(".ral")) {
        ImportName(tmp.dropRight(4))
      } else {
        ImportName(tmp)
      }
    }
  }

  case class ParsedImport(name: ImportName, index: Int, fullParse: String, fullParseIndex: Int)

  object Lexer {
    implicit val whitespace: P[_] => P[Unit] = { implicit ctx: P[_] => ralph.Lexer.emptyChars(ctx) }

    def importKey[Unknown:P]: P[Unit] = P("import")

    def interfaceName[Unknown: P]: P[Unit] = {
      implicit val whitespace: P[_] => P[Unit] = fastparse.NoWhitespace.noWhitespaceImplicit
      P((CharIn("a-z") | "/" | "_" | "-" | "." | " ").rep)
    }

    def importValue[Unknown: P]: P[Unit] = {
      implicit val whitespace: P[_] => P[Unit] = fastparse.NoWhitespace.noWhitespaceImplicit
      P("\"" ~ interfaceName ~ "\"")
    }

    def fullImport[Unknown:P]: P[ParsedImport] = {
      implicit val whitespace: P[_] => P[Unit] = fastparse.NoWhitespace.noWhitespaceImplicit
      val parsedResult = P(importKey.! ~ ralph.Lexer.emptyChars.! ~ importValue.!)

      val globalIndex = parsedResult.index

      parsedResult.map { case (key,spaces,value) =>
        val fullParse = s"""$key$spaces$value"""
        val endIndex = parsedResult.index
        val fullParseIndex = endIndex - fullParse.size
        val importValue = ImportName.cleaned(value)
        val index = globalIndex - value.size + 1 //+ 1 for the front "
        ParsedImport(importValue, index, fullParse, fullParseIndex)
      }
    }

    def imports[Unknown:P]: P[Seq[ParsedImport]] = P(Start ~ (fullImport | AnyChar).rep).map(_.collect{ case p:ParsedImport => p})
  }


  /*
   * Parse source code and find all imports.
   * Imports are resolved with their corresponding file, if not found we return an error.
   * We pass through all imports so we can have the full list of non-existing import
   */
  def extractStdImports(code: String): Either[Seq[CompilerMessage.AnyError], Result] = {
    fastparse.parse(code, Lexer.imports(_)) match {
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
