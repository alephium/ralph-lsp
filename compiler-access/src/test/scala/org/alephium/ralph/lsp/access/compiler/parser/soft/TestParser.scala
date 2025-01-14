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
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.utils.Node
import org.scalatest.matchers.should.Matchers._

object TestParser {

  def parseSoft(code: String): SoftAST.BlockBody =
    runSoftParser(SoftParser.parse(_))(code)

  def parseAnnotation(code: String): SoftAST.Annotation =
    runSoftParser(AnnotationParser.parseOrFail(_))(code)

  def parseTemplate(code: String): SoftAST.Template =
    runSoftParser(TemplateParser.parseOrFail(_))(code)

  def parseFunction(code: String): SoftAST.Function =
    runSoftParser(FunctionParser.parseOrFail(_))(code)

  def parseTuple(code: String): SoftAST.Tuple =
    runSoftParser(TupleParser.parse(_))(code)

  def parseBlockClause(mandatory: Boolean)(code: String): SoftAST.BlockClause =
    runSoftParser(BlockParser.clause(mandatory)(_))(code)

  def parseBlockBody(code: String): SoftAST.BlockBody =
    runSoftParser(BlockParser.body(_))(code)

  def parseComment(code: String): SoftAST.Comments =
    runSoftParser(CommentParser.parseOrFail(_))(code)

  def parseType(code: String): SoftAST.TypeAST =
    runSoftParser(TypeParser.parse(_))(code)

  def parseReservedToken(remove: Token.Reserved*)(code: String): Token.Reserved =
    runAnyParser(TokenParser.Reserved(remove: _*)(_))(code)

  def parseInfixOperatorOrFail(code: String): SoftAST.TokenDocumented[Token.InfixOperator] =
    runAnyParser(TokenParser.InfixOperatorOrFail(_))(code)

  def parseReservedTokenOrError(remove: Token.Reserved*)(code: String): Either[Parsed.Failure, Token.Reserved] =
    runAnyParserOrError(TokenParser.Reserved(remove: _*)(_))(code)

  def parseIdentifier(code: String): SoftAST.IdentifierAST =
    runSoftParser(IdentifierParser.parseOrFail(_))(code)

  def parseBoolean(code: String): SoftAST.TokenExpression[Token.PrimitiveBoolean] =
    runSoftParser(BooleanParser.parseOrFail(_))(code)

  def parseNumber(code: String): SoftAST.Number =
    runSoftParser(NumberParser.parseOrFail(_))(code)

  def findAnnotation(identifier: String)(code: String): Option[SoftAST.Annotation] =
    findAnnotation(
      identifier = identifier,
      ast = parseSoft(code)
    )

  def findAnnotation(
      identifier: String,
      ast: SoftAST): Option[SoftAST.Annotation] =
    ast
      .toNode()
      .walkDown
      .collectFirst {
        case Node(annotation @ SoftAST.Annotation(_, _, _, id: SoftAST.Identifier, _, _, _), _) if id.code.text == identifier =>
          annotation
      }

  def findFirstComment(body: SoftAST): Option[SoftAST.Comments] =
    body
      .toNode()
      .walkDown
      .collectFirst {
        case Node(comments @ SoftAST.Comments(_, _, _, _), _) =>
          comments
      }

  private def runSoftParser[T <: SoftAST](parser: P[_] => P[T])(code: String): T = {
    val ast =
      runAnyParser(parser)(code)

    val astToCode =
      ast.toCode()

    // AST returned by the parser should ALWAYS emit code identical to the inputted code.
    // Always assert this for each test case.
    try
      astToCode shouldBe code
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

  private def runAnyParser[T](parser: P[_] => P[T])(code: String): T = {
    val result: Either[Parsed.Failure, T] =
      runAnyParserOrError(parser)(code)

    // invoke .get to ensure that the parser should NEVER fail
    result match {
      case Left(error) =>
        // Print a formatted error so it's easier to debug.
        val throwable = CompilerError.FastParseError(error)
        fail(throwable.toFormatter().format(Some(Console.RED)), throwable)

      case Right(ast) =>
        ast
    }
  }

  private def runAnyParserOrError[T](parser: P[_] => P[T])(code: String): Either[Parsed.Failure, T] =
    fastparse.parse(code, parser) match {
      case Parsed.Success(ast, _) =>
        Right(ast)

      case failure: Parsed.Failure =>
        Left(failure)
    }

}
