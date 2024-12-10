// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse.{P, Parsed}
import org.alephium.ralph.lsp.access.compiler.message.error.FastParseError
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.scalatest.matchers.should.Matchers._

object TestParser {

  def parseSoft(code: String): SoftAST.BlockBody =
    runParser(SoftParser.parse(_))(code)

  def parseTemplate(code: String): SoftAST.Template =
    runParser(TemplateParser.parseOrFail(_))(code)

  def parseFunction(code: String): SoftAST.Function =
    runParser(FunctionParser.parseOrFail(_))(code)

  def parseTuple(code: String): SoftAST.Tuple =
    runParser(TupleParser.parse(_))(code)

  def parseBlockClause(mandatory: Boolean)(code: String): SoftAST.BlockClause =
    runParser(BlockParser.clause(mandatory)(_))(code)

  def parseBlockBody(code: String): SoftAST.BlockBody =
    runParser(BlockParser.body(_))(code)

  def parseComment(code: String): SoftAST.Comments =
    runParser(CommentParser.parseOrFail(_))(code)

  def parseType(code: String): SoftAST.TypeAST =
    runParser(TypeParser.parse(_))(code)

  private def runParser[T <: SoftAST](parser: P[_] => P[T])(code: String): T = {
    // invoke .get to ensure that the parser should NEVER fail
    val result =
      fastparse.parse(code, parser) match {
        case Parsed.Success(ast, _) =>
          Right(ast)

        case failure: Parsed.Failure =>
          Left(FastParseError(failure))
      }

    val ast =
      result match {
        case Left(error) =>
          // Print a formatted error so it's easier to debug.
          fail(error.error.toFormatter().format(Some(Console.RED)))

        case Right(ast) =>
          ast
      }

    val astToCode =
      ast.toCode()

    // AST returned by the parser should ALWAYS emit code identical to the inputted code.
    // Always assert this for each test case.
    try
      code shouldBe astToCode
    catch {
      case throwable: Throwable =>
        // Print debug info for cases where the emitted code differs from the input code.
        println("Input Code:")
        println(code)
        println("\nAST to Code:")
        println(astToCode)
        throw throwable
    }

    ast
  }

}
