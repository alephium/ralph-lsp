// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse.{P, Parsed}
import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.utils.Node
import org.alephium.ralph.SourceIndex
import org.scalatest.matchers.should.Matchers._

import scala.util.Random

object TestParser {

  def parseSoft(code: String): SoftAST.RootBlock =
    runSoftParser(SoftParser.parse(_))(code)

  def parseAnnotation(code: String): SoftAST.Annotation =
    runSoftParser(AnnotationParser.parseOrFail(_))(code)

  def parseVariableDeclaration(code: String): SoftAST.VariableDeclaration =
    runSoftParser(VariableDeclarationParser.parseOrFail(_))(code)

  def parseAssignment(code: String): SoftAST.Assignment =
    runSoftParser(AssignmentParser.parseOrFail(_))(code)

  def parseMutableBinding(code: String): SoftAST.MutableBinding =
    runSoftParser(MutableBindingParser.parseOrFail(_))(code)

  def parseTemplate(code: String): SoftAST.Template =
    runSoftParser(TemplateParser.parseOrFail(_))(code)

  def parseFunction(code: String): SoftAST.Function =
    runSoftParser(FunctionParser.parseOrFail(_))(code)

  def parseTuple(code: String): SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type, Token.Comma.type] =
    runSoftParser(TupleParser.parse(_))(code)

  def parseNonEmptyTuple(code: String): SoftAST.Group[Token.OpenParen.type, Token.CloseParen.type, Token.Comma.type] =
    runSoftParser(TupleParser.parseOrFail(assertNonEmpty = true)(_))(code)

  def parseBlock(code: String): SoftAST.Block =
    runSoftParser(BlockParser.parseOrFail(_))(code)

  def parseRootBlock(code: String): SoftAST.RootBlock =
    runSoftParser(RootBlockParser.parseOrFail(_))(code)

  def parseComment(code: String): SoftAST.Comments =
    runSoftParser(CommentParser.parseOrFail(_))(code)

  def parseReservedToken()(code: String): Token.Reserved =
    runAnyParser(TokenParser.Reserved()(_))(code)

  def parseInfixOperatorOrFail(code: String): SoftAST.TokenDocumented[Token.InfixOperator] =
    runAnyParser(TokenParser.InfixOperatorOrFail(_))(code)

  def parseReservedTokenOrError()(code: String): Either[Parsed.Failure, Token.Reserved] =
    runAnyParserOrError(TokenParser.Reserved()(_))(code)

  def parseIdentifier(code: String): SoftAST.IdentifierAST =
    runSoftParser(IdentifierParser.parseOrFail(_))(code)

  def parseBoolean(code: String): SoftAST.TokenExpression[Token.PrimitiveBoolean] =
    runSoftParser(BooleanParser.parseOrFail(_))(code)

  def parseNumber(code: String): SoftAST.Number =
    runSoftParser(NumberParser.parseOrFail(_))(code)

  /** Run the parser but perform no input code check */
  def parseNumberNoCodeCheck(code: String): SoftAST.Number =
    runAnyParser(NumberParser.parseOrFail(_))(code)

  def parseBString(code: String): SoftAST.BString =
    runSoftParser(BStringParser.parseOrFail(_))(code)

  def parseStringLiteral(code: String): SoftAST.StringLiteral =
    runSoftParser(StringLiteralParser.parseOrFail(_))(code)

  def parseStringInterpolation(code: String): SoftAST.StringInterpolation =
    runSoftParser(StringInterpolationParser.parseOrFail(_))(code)

  def parseImport(code: String): SoftAST.Import =
    runSoftParser(ImportParser.parseOrFail(_))(code)

  def parseEvent(code: String): SoftAST.Event =
    runSoftParser(EventParser.parseOrFail(_))(code)

  def parseStruct(code: String): SoftAST.Struct =
    runSoftParser(StructParser.parseOrFail(_))(code)

  def parseStructConstructor(code: String): SoftAST.StructConstructor =
    runSoftParser(StructConstructorParser.parseOrFail(_))(code)

  def parseStructConstructorNoCodeCheck(code: String): SoftAST.StructConstructor =
    runAnyParser(StructConstructorParser.parseOrFail(_))(code)

  def parseEnum(code: String): SoftAST.Enum =
    runSoftParser(EnumParser.parseOrFail(_))(code)

  def parseTypeAssignment(code: String): SoftAST.TypeAssignment =
    runSoftParser(TypeAssignmentParser.parseOrFail(_))(code)

  def parseInheritance(code: String): SoftAST.Inheritance =
    runSoftParser(InheritanceParser.parseOrFail(_))(code)

  def parseAccessModifier(code: String): SoftAST.AccessModifier =
    runSoftParser(AccessModifierParser.parseOrFail(_))(code)

  def parseFor(code: String): SoftAST.For =
    runSoftParser(ForParser.parseOrFail(_))(code)

  def parseWhile(code: String): SoftAST.While =
    runSoftParser(WhileParser.parseOrFail(_))(code)

  def parseMethodCall(code: String): SoftAST.MethodCall =
    runSoftParser(MethodCallParser.parseOrFail(_))(code)

  def parseReturn(code: String): SoftAST.Return =
    runSoftParser(ReturnParser.parseOrFail(_))(code)

  def parseInfixCall(code: String): SoftAST.InfixExpression =
    runSoftParser(InfixCallParser.parseOrFail(_))(code)

  def parseConst(code: String): SoftAST.Const =
    runSoftParser(ConstParser.parseOrFail(_))(code)

  def parseIfElse(code: String): SoftAST.IfElse =
    runSoftParser(IfElseParser.parseOrFail(_))(code)

  def parseElse(code: String): SoftAST.Else =
    runSoftParser(ElseParser.parseOrFail(_))(code)

  def parseEmit(code: String): SoftAST.Emit =
    runSoftParser(EmitParser.parseOrFail(_))(code)

  def parseByteVec(code: String): SoftAST.ByteVec =
    runSoftParser(ByteVecParser.parseOrFail(_))(code)

  def parseArray(code: String): SoftAST.ArrayAST =
    runSoftParser(ArrayParser.parseOrFail(_))(code)

  def parseArrayAccess(code: String): SoftAST.ArrayAccess =
    runSoftParser(ArrayAccessParser.parseOrFail(_))(code)

  def parseMapAssignment(code: String): SoftAST.MapAssignment =
    runSoftParser(MapAssignmentParser.parseOrFail(_))(code)

  def parseAssetApproval(code: String): SoftAST.AssetApproval =
    runSoftParser(AssetApprovalParser.parseOrFail(_))(code)

  def parseArrowAssignment(code: String): SoftAST.ArrowAssignment =
    runSoftParser(ArrowAssignmentParser.parseOrFail(_))(code)

  def parseReferenceCall(code: String): SoftAST.ReferenceCall =
    runSoftParser(ReferenceCallParser.parseOrFail(_))(code)

  /**
   * Test the result of [[SoftAST.deepCopy]] on the given AST.
   */
  def testDeepCopy[A <: SoftAST](ast: A): A = {
    val newSourceIndex      = new SourceIndex(Random.nextInt(100), Random.nextInt(1000), None) // Generate a random SourceIndex
    val newAST              = ast.deepCopy(newSourceIndex)                                     // Copy the tree with new SourceIndex
    val newASTSourceIndexes = newAST.toNode.walkDown.map(_.data.index).distinct.toList         // Collect distinct SourceIndexes from the update tree
    newASTSourceIndexes should contain only newSourceIndex // It should contain only the one new SourceIndex
    newAST
  }

  def findAnnotation(identifier: String)(code: String): Option[SoftAST.Annotation] =
    findAnnotation(
      identifier = identifier,
      ast = parseSoft(code)
    )

  def findAnnotation(
      identifier: String,
      ast: SoftAST): Option[SoftAST.Annotation] =
    ast
      .toNode
      .walkDown
      .collectFirst {
        case Node(annotation @ SoftAST.Annotation(_, _, _, id: SoftAST.Identifier, _, _, _), _) if id.code.text == identifier =>
          annotation
      }

  def findFirstComment(ast: SoftAST): Option[SoftAST.Comments] =
    ast
      .toNode
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

    // Piggyback on all existing parser test-cases to test the `SoftAST.deepCody` function for different cases.
    testDeepCopy(ast)

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
