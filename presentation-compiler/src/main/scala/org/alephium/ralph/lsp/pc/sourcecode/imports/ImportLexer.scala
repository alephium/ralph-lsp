package org.alephium.ralph.lsp.pc.sourcecode.imports

import fastparse._
import fastparse.Parsed
import org.alephium.ralph

object ImportLexer {
  implicit val whitespace: P[_] => P[Unit] = { implicit ctx: P[_] => ralph.Lexer.emptyChars(ctx) }

  def importKey[Unknown:P]: P[Unit] = P("import")

  def interfaceName[Unknown: P]: P[Unit] = {
    implicit val whitespace: P[_] => P[Unit] = fastparse.NoWhitespace.noWhitespaceImplicit
    P((CharIn("a-z") | "/" | "_" | "-" | "." | " " | "\n").rep)
  }

  def importValue[Unknown: P]: P[Unit] = {
    implicit val whitespace: P[_] => P[Unit] = fastparse.NoWhitespace.noWhitespaceImplicit
    P("\"" ~ interfaceName ~ "\"")
  }

  def fullImport[Unknown:P]: P[ImportState.Parsed] = {
    implicit val whitespace: P[_] => P[Unit] = fastparse.NoWhitespace.noWhitespaceImplicit
    val parsedResult = P(importKey.! ~ ralph.Lexer.emptyChars.! ~ importValue.!)

    val globalIndex = parsedResult.index

    parsedResult.map { case (key,spaces,value) =>
      val fullParse = s"""$key$spaces$value"""
      val endIndex = parsedResult.index
      val fullParseIndex = endIndex - fullParse.size
      val importValue = ImportName.cleaned(value)
      val index = globalIndex - value.size + 1 //+ 1 for the front "
      ImportState.Parsed(importValue, index, fullParse, fullParseIndex)
    }
  }

  def imports[Unknown:P]: P[Seq[ImportState.Parsed]] = P(Start ~ (fullImport | AnyChar).rep).map(_.collect{ case p:ImportState.Parsed => p})
}
