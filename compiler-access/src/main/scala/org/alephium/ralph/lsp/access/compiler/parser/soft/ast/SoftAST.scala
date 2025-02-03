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

package org.alephium.ralph.lsp.access.compiler.parser.soft.ast

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST.collectASTs
import org.alephium.ralph.lsp.utils.Node

sealed trait SoftAST extends Product { self =>

  def index: SourceIndex

  final def children(): Iterator[SoftAST] =
    productIterator flatMap collectASTs

  /**
   * Similar to [[org.alephium.ralph.lsp.access.compiler.ast.Tree.Source.rootNode]],
   * this tree creation is also lazily evaluated and expected to have concurrent access.
   *
   * TODO: Move these caches to solutions like Caffeine.
   */
  final lazy val toNode: Node[this.type, SoftAST] =
    Node(
      data = self,
      children = children().map(_.toNode).toSeq
    )

  final def toCode(): String =
    toNode.toCode()

  final def toStringTree(): String =
    toNode.toStringTree()

  def toStringPretty(): String =
    s"${self.getClass.getSimpleName}: ${self.index}"

}

object SoftAST {

  implicit class NodeSoftASTExtensions(val node: Node[SoftAST, SoftAST]) extends AnyVal {

    def toCode(): String =
      node.walkDown.foldLeft("") {
        case (code, Node(ast: Code, _)) =>
          code + ast.text

        case (code, _) =>
          code
      }

    def toStringTree(): String =
      node.toStringTree(_.toStringPretty())

  }

  sealed trait BodyPartAST extends SoftAST

  sealed trait ExpressionAST extends BodyPartAST

  case class ExpressionExpected(
      index: SourceIndex)
    extends ExpectedErrorAST("Symbol")
       with ExpressionAST

  /**
   * ASTs that store code.
   */
  sealed trait CodeAST extends SoftAST {

    def code: Code

  }

  /**
   * [[CodeAST]] instances that can contain documentation.
   */
  sealed trait CodeDocumentedAST extends CodeAST {

    def documentation: Option[Comments]

  }

  /**
   * Has an [[Token]] type associates with it.
   */
  sealed trait TokenAST[+T <: Token] extends CodeAST {

    def code: CodeToken[T]

  }

  abstract class ErrorAST(val message: String) extends SoftAST {

    // Also display the error message in the Tree representation
    final override def toStringPretty(): String =
      s"""${this.getClass.getSimpleName}("$message"): ${this.index}"""

  }

  abstract class ExpectedErrorAST(element: String)   extends ErrorAST(s"$element expected")
  abstract class TokenExpectedErrorAST(token: Token) extends ExpectedErrorAST(s"'${token.lexeme}'")

  sealed trait TokenDocExpectedAST[+T <: Token] extends SoftAST

  /**
   * Represents a token that may contain code comments or documentation.
   *
   * @param index         Source index of the token, including its documentation.
   * @param documentation Optional documentation (e.g. comments) preceding the token
   * @param code          The token itself.
   * @tparam T The specific type of token.
   */
  case class TokenDocumented[+T <: Token](
      index: SourceIndex,
      documentation: Option[Comments],
      code: CodeToken[T])
    extends TokenDocExpectedAST[T]
       with CodeDocumentedAST

  case class TokenUndocumented[+T <: Token](code: CodeToken[T]) extends TokenAST[T] {

    override def index: SourceIndex =
      code.index

  }

  case class TokenExpected[T <: Token](
      index: SourceIndex,
      token: T)
    extends TokenExpectedErrorAST(token)
       with TokenDocExpectedAST[T]

  /**
   * Represents a token that is also an expression, e.g. `true` & `false`.
   *
   * @param token A token that is also an expression.
   * @tparam T Type of token.
   */
  case class TokenExpression[+T <: Token.Expression](token: TokenDocumented[T]) extends ExpressionAST {

    override def index: SourceIndex =
      token.index

  }

  case class Template(
      index: SourceIndex,
      templateType: TokenDocumented[Token.TemplateDefinition],
      preIdentifierSpace: Option[Space],
      identifier: IdentifierAST,
      preParamSpace: Option[Space],
      params: Option[Group[Token.OpenParen.type, Token.CloseParen.type]],
      postParamSpace: Option[Space],
      inheritance: Seq[Inheritance],
      block: BlockClause)
    extends BodyPartAST

  case class Event(
      index: SourceIndex,
      eventToken: TokenDocumented[Token.Event.type],
      preIdentifierSpace: Option[Space],
      identifier: IdentifierAST,
      preParamSpace: Option[Space],
      params: Group[Token.OpenParen.type, Token.CloseParen.type])
    extends BodyPartAST

  case class Struct(
      index: SourceIndex,
      structToken: TokenDocumented[Token.Struct.type],
      preIdentifierSpace: Option[Space],
      identifier: IdentifierAST,
      preParamSpace: Option[Space],
      params: Group[Token.OpenCurly.type, Token.CloseCurly.type])
    extends BodyPartAST

  case class Import(
      index: SourceIndex,
      importToken: TokenDocumented[Token.Import.type],
      postImportSpace: Option[Space],
      string: Option[StringLiteral])
    extends BodyPartAST

  /** Syntax: `implements or extends contract(arg1, arg2 ...)` */
  case class Inheritance(
      index: SourceIndex,
      inheritanceType: TokenDocumented[Token.Inheritance],
      postInheritanceTypeSpace: Option[Space],
      reference: ReferenceCallOrIdentifier,
      postConstructorCallSpace: Option[Space])
    extends SoftAST

  case class BlockClause(
      index: SourceIndex,
      openCurly: TokenDocExpectedAST[Token.OpenCurly.type],
      preBodySpace: Option[Space],
      body: BlockBody,
      postBodySpace: Option[Space],
      closeCurly: TokenDocExpectedAST[Token.CloseCurly.type])
    extends ExpressionAST

  case class BlockBody(
      index: SourceIndex,
      prePartsSpace: Option[Space],
      parts: Seq[BlockBodyPart])
    extends SoftAST

  case class BlockBodyPart(
      index: SourceIndex,
      part: BodyPartAST,
      postPartSpace: Option[Space])
    extends SoftAST

  case class Function(
      index: SourceIndex,
      annotations: Seq[Annotation],
      postAnnotationSpace: Option[Space],
      pub: Option[AccessModifier],
      fn: TokenDocumented[Token.Fn.type],
      preSignatureSpace: Option[Space],
      signature: FunctionSignature,
      postSignatureSpace: Option[Space],
      block: Option[BlockClause])
    extends BodyPartAST

  case class FunctionSignature(
      index: SourceIndex,
      fnName: IdentifierAST,
      preParamSpace: Option[Space],
      params: Group[Token.OpenParen.type, Token.CloseParen.type],
      postParamSpace: Option[Space],
      returned: FunctionReturnAST)
    extends SoftAST

  /**
   * Represents a comma separated list of expressions.
   *
   * This list of expressions is parsed by the following syntax:
   *  - Struct `{ a, b: Type, mut c: C }`
   *  - Annotation `@using(a = b)`
   *  - Contract | TxScript`(a, b, c)`
   *  - fn `(a: Type, b: Type, c)`
   *  - `enum` etc
   */
  case class Group[O <: Token, C <: Token](
      index: SourceIndex,
      openToken: TokenDocExpectedAST[O],
      preHeadExpressionSpace: Option[Space],
      headExpression: Option[ExpressionAST],
      postHeadExpressionSpace: Option[Space],
      tailExpressions: Seq[GroupTail],
      closeToken: TokenDocExpectedAST[C])
    extends ExpressionAST

  /** Comma separated tail expressions of a [[Group]] */
  case class GroupTail(
      index: SourceIndex,
      comma: TokenDocumented[Token.Comma.type],
      preExpressionSpace: Option[Space],
      expression: ExpressionAST,
      postExpressionSpace: Option[Space])
    extends SoftAST

  sealed trait FunctionReturnAST extends SoftAST

  case class FunctionReturn(
      index: SourceIndex,
      forwardArrow: TokenDocExpectedAST[Token.ForwardArrow.type],
      space: Option[Space],
      tpe: ExpressionAST)
    extends FunctionReturnAST

  case class FunctionReturnExpected(
      index: SourceIndex)
    extends TokenExpectedErrorAST(Token.ForwardArrow)
       with FunctionReturnAST

  sealed trait IdentifierAST extends ReferenceCallOrIdentifier

  case class Identifier(
      index: SourceIndex,
      documentation: Option[Comments],
      code: CodeString)
    extends IdentifierAST
       with CodeDocumentedAST

  case class IdentifierExpected(
      index: SourceIndex)
    extends ExpectedErrorAST("Identifier")
       with IdentifierAST

  case class Comments(
      index: SourceIndex,
      preCommentSpace: Option[Space],
      comments: Seq[Comment],
      postCommentSpace: Option[Space])
    extends BodyPartAST

  case class Comment(
      index: SourceIndex,
      doubleForwardSlash: TokenUndocumented[Token.DoubleForwardSlash.type],
      preTextSpace: Option[Space],
      text: Option[CodeString],
      postTextSpace: Option[Space])
    extends BodyPartAST

  case class Unresolved(
      index: SourceIndex,
      documentation: Option[Comments],
      code: CodeString)
    extends ErrorAST(s"Cannot resolve '${code.text}'")
       with CodeDocumentedAST
       with BodyPartAST

  sealed trait ReferenceCallOrIdentifier extends ExpressionAST

  case class ReferenceCall(
      index: SourceIndex,
      reference: IdentifierAST,
      preArgumentsSpace: Option[Space],
      arguments: Group[Token.OpenParen.type, Token.CloseParen.type])
    extends ReferenceCallOrIdentifier

  case class InfixExpression(
      index: SourceIndex,
      leftExpression: ExpressionAST,
      preOperatorSpace: Option[Space],
      operator: TokenDocumented[Token.InfixOperator],
      postOperatorSpace: Option[Space],
      rightExpression: ExpressionAST)
    extends ExpressionAST

  case class MethodCall(
      index: SourceIndex,
      leftExpression: ExpressionAST,
      preDotSpace: Option[Space],
      dotCalls: Seq[DotCall])
    extends ExpressionAST

  case class DotCall(
      index: SourceIndex,
      dot: TokenDocumented[Token.Dot.type],
      postDotSpace: Option[Space],
      rightExpression: ReferenceCallOrIdentifier)
    extends SoftAST

  case class ReturnStatement(
      index: SourceIndex,
      returnToken: TokenDocumented[Token.Return.type],
      preExpressionSpace: Option[Space],
      rightExpression: ExpressionAST)
    extends ExpressionAST

  case class ForStatement(
      index: SourceIndex,
      forToken: TokenDocumented[Token.For.type],
      postForSpace: Option[Space],
      openParen: TokenDocExpectedAST[Token.OpenParen.type],
      postOpenParenSpace: Option[Space],
      expression1: ExpressionAST,
      postExpression1Space: Option[Space],
      postExpression1Semicolon: TokenDocExpectedAST[Token.Semicolon.type],
      postExpression1SemicolonSpace: Option[Space],
      expression2: ExpressionAST,
      postExpression2Space: Option[Space],
      postExpression2Semicolon: TokenDocExpectedAST[Token.Semicolon.type],
      postExpression2SemicolonSpace: Option[Space],
      expression3: ExpressionAST,
      postExpression3Space: Option[Space],
      closeParen: TokenDocExpectedAST[Token.CloseParen.type],
      postCloseParenSpace: Option[SoftAST.Space],
      block: SoftAST.BlockClause)
    extends ExpressionAST

  case class WhileStatement(
      index: SourceIndex,
      whileToken: TokenDocumented[Token.While.type],
      postWhileSpace: Option[Space],
      openParen: TokenDocExpectedAST[Token.OpenParen.type],
      postOpenParenSpace: Option[Space],
      expression: ExpressionAST,
      postExpressionSpace: Option[Space],
      closeParen: TokenDocExpectedAST[Token.CloseParen.type],
      postCloseParenSpace: Option[SoftAST.Space],
      block: SoftAST.BlockClause)
    extends ExpressionAST

  case class Assignment(
      index: SourceIndex,
      expressionLeft: ExpressionAST,
      postIdentifierSpace: Option[Space],
      equalToken: TokenDocumented[Token.Equal.type],
      postEqualSpace: Option[Space],
      expressionRight: ExpressionAST)
    extends ExpressionAST

  case class TypeAssignment(
      index: SourceIndex,
      name: ExpressionAST,
      preColonSpace: Option[Space],
      colon: TokenDocumented[Token.Colon.type],
      postColonSpace: Option[Space],
      tpe: ExpressionAST)
    extends ExpressionAST

  case class AssignmentAccessModifier(
      index: SourceIndex,
      token: TokenDocumented[Token.DataDefinition],
      postTokenSpace: Space)
    extends ExpressionAST

  case class AccessModifier(
      index: SourceIndex,
      pub: TokenDocumented[Token.Pub.type],
      postTokenSpace: Option[Space])
    extends ExpressionAST

  case class VariableDeclaration(
      index: SourceIndex,
      modifiers: Seq[AssignmentAccessModifier],
      assignment: Assignment)
    extends ExpressionAST

  /** Syntax: mut [identifier] */
  case class MutableBinding(
      index: SourceIndex,
      mut: TokenDocumented[Token.Mut.type],
      space: Space,
      identifier: Identifier)
    extends ExpressionAST

  case class Annotation(
      index: SourceIndex,
      at: TokenDocumented[Token.At.type],
      preIdentifierSpace: Option[Space],
      identifier: IdentifierAST,
      postIdentifierSpace: Option[Space],
      tuple: Option[Group[Token.OpenParen.type, Token.CloseParen.type]],
      postTupleSpace: Option[Space])
    extends ExpressionAST

  case class Number(
      index: SourceIndex,
      documentation: Option[Comments],
      number: CodeString,
      space: Option[Space],
      unit: Option[TokenDocumented[Token.AlphLowercase.type]])
    extends ExpressionAST

  case class BString(
      index: SourceIndex,
      b: TokenDocumented[Token.B.type],
      postBSpace: Option[Space],
      startTick: TokenDocumented[Token.Tick.type],
      text: Option[CodeString],
      endTick: TokenDocExpectedAST[Token.Tick.type])
    extends ExpressionAST

  case class StringLiteral(
      index: SourceIndex,
      startQuote: TokenDocumented[Token.Quote.type],
      head: Option[CodeStringAST],
      tail: Seq[Path],
      endQuote: TokenDocExpectedAST[Token.Quote.type])
    extends ExpressionAST

  case class Path(
      index: SourceIndex,
      slash: TokenDocumented[Token.ForwardSlash.type],
      text: CodeStringAST)
    extends SoftAST

  case class Space(
      code: CodeString)
    extends CodeAST {

    override def index: SourceIndex =
      code.index

  }

  sealed trait Code extends SoftAST {

    def text: String

    private def textPretty: String =
      text
        .replaceAll("\r", "\\\\r") // Replace carriage with \\r
        .replaceAll("\n", "\\\\n") // Replace newline with \\n
        .replaceAll("\t", "\\\\t") // Replace tab with \\t)

    override def toStringPretty(): String =
      s"${getClass.getSimpleName}(\"$textPretty\"): $index"

  }

  sealed trait CodeStringAST extends SoftAST

  /**
   * Represents a string within a segment of code.
   *
   * @param index Source index of the code string.
   * @param text  String content of the code
   */
  case class CodeString(
      index: SourceIndex,
      text: String)
    extends CodeStringAST
       with Code

  /** Represents a location where a code symbol is expected, but an empty value was provided. */
  case class CodeStringExpected(
      index: SourceIndex)
    extends ExpectedErrorAST("Symbol")
       with CodeStringAST

  /**
   * Represents a token within a segment of code.
   *
   * @param index Source index of the token
   * @param token Token instance representing the parsed code
   * @tparam T Specific type of token.
   */
  case class CodeToken[+T <: Token](
      index: SourceIndex,
      token: T)
    extends Code {

    override def text: String =
      token.lexeme

  }

  private def collectASTs: PartialFunction[Any, Seq[SoftAST]] = {
    case ast: SoftAST =>
      Seq(ast)

    case Some(any) =>
      collectASTs(any)

    case seq: Seq[_] =>
      seq flatMap collectASTs

    case _ =>
      Seq.empty
  }

}
