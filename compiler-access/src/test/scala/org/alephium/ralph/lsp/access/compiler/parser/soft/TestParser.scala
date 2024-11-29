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
import org.alephium.ralph.error.CompilerError
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

  def parseParameter(code: String): SoftAST.ParameterClauseAST =
    runParser(ParameterParser.parse(_))(code)

  def parseBlockClause(mandatory: Boolean)(code: String): SoftAST.BlockClause =
    runParser(BlockParser.clause(mandatory)(_))(code)

  def parseBlockBody(code: String): SoftAST.BlockBody =
    runParser(BlockParser.body(_))(code)

  def parseComment(code: String): SoftAST.Comment =
    runParser(CommentParser.parseOrFail(_))(code)

  def parseType(code: String): SoftAST.TypeAST =
    runParser(TypeParser.parse(_))(code)

  def parseOrFailArgument(code: String): Either[CompilerError.FastParseError, SoftAST.Arguments] =
    runParserOrFail(ArgumentParser.parseOrFail(_))(code)

  def parseArgument(code: String): SoftAST.Arguments =
    runParser(ArgumentParser.parse(_))(code)

  /**
   * Executes the parser ensuring no [[CompilerError.FastParseError]] error.
   * If a [[CompilerError.FastParseError]] error does occur, a formatted error message is printed.
   *
   * @param parser The parser function to execute.
   * @param code The code to run the parser on.
   * @tparam T The type of parsed result.
   * @return A successfully parsed instance of type [[T]].
   */
  private def runParser[T <: SoftAST](parser: P[_] => P[T])(code: String): T =
    runParserOrFail(parser)(code) match {
      case Left(error) =>
        // Print a formatted error so it's easier to debug.
        fail(error.toFormatter().format(Some(Console.RED)))

      case Right(ast) =>
        ast
    }

  /**
   * Executes the parser ensuring that the resulting AST's code matches the provided/input code.
   *
   * @param parser The parser function to execute.
   * @param code The code to run the parser on.
   * @tparam T The type of parsed result.
   * @return An `Either` containing:
   *         - A successfully parsed instance of type [[T]] if parsing succeeds.
   *         - Or, the FastParse parsing error [[CompilerError.FastParseError]].
   */
  private def runParserOrFail[T <: SoftAST](parser: P[_] => P[T])(code: String): Either[CompilerError.FastParseError, T] = {
    // invoke .get to ensure that the parser should NEVER fail
    val result =
      fastparse.parse(code, parser) match {
        case Parsed.Success(ast, _) =>
          Right(ast)

        case failure: Parsed.Failure =>
          Left(FastParseError(failure))
      }

    result
      .left
      .map(_.error)
      .map {
        ast =>
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

}
