package org.alephium.ralph.lsp.pc.sourcecode

import fastparse.Parsed
import org.alephium.api.model.CompileProjectResult
import org.alephium.ralph._
import org.alephium.ralphc.{Config, MetaInfo, Compiler => RalphC}
import org.alephium.ralph.lsp.compiler.message.CompilerMessage

import java.net.URI
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Using}
import scala.util.matching.Regex

object ImportHandler {

  case class Result(code: String, imports: Map[String, String])

  private val importRegex = """\s*import\s+"([^".\/]+\/[^"]*[a-z][a-z_0-9])"\s*\n""".r

  //Extract imports and replace them with empty string in code to keep line numbers
  def extractStdImports(initialCode: String): Either[CompilerMessage.AnyError, Result] = {
    val (code, imports) = importRegex.findAllMatchIn(initialCode)
      .foldLeft(initialCode, Map.empty[String,String]) { case ((code, imports), pattern) =>
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
            (code.replaceFirst(pattern.matched, pattern.matched.replace("import","//")), imports ++ Map(importValue-> interfaceCode))
          case None =>
            (code, imports)
        }
      }

      Right(Result(code, imports))
  }
}
